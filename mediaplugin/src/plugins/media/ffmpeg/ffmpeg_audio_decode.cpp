/* UltraStar Deluxe - Karaoke Game
 *
 * UltraStar Deluxe is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; see the file COPYING. If not, write to
 * the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 */

/*******************************************************************************
 *
 * This code is primarily based upon -
 *   http://www.dranger.com/ffmpeg/ffmpegtutorial_all.html
 *
 *   and tutorial03.c
 *
 *   http://www.inb.uni-luebeck.de/~boehme/using_libavcodec.html
 *
 *******************************************************************************/

#include "ffmpeg_audio_decode.h"
#include <string>
#include <sstream>
#include <cmath>

// show FFmpeg specific debug output
//#define DEBUG_FFMPEG_DECODE

// FFmpeg is very verbose and shows a bunch of errors.
// Those errors (they can be considered as warnings by us) can be ignored
// as they do not give any useful information.
// There is no solution to fix this except for turning them off.
//#define ENABLE_FFMPEG_ERROR_OUTPUT

#define MAX_AUDIOQ_SIZE (5 * 16 * 1024)

/* FFmpegDecodeStream */

FFmpegAudioDecodeStream::FFmpegAudioDecodeStream() :
	_eofState(false),
	_errorState(false),
	_quitRequest(false),
	_parserLocked(false),
	_parserPauseRequestCount(0),
	_seekRequest(false),
	_seekFlags(0),
	_seekPos(0),
	_seekFlush(false),
	_loop(false),
	_formatCtx(NULL),
	_codecCtx(NULL),
	_codec(NULL),
	_audioStreamIndex(-1),
	_audioStream(NULL),
	_audioStreamPos(0.0),
	_decoderLocked(false),
	_decoderPauseRequestCount(0),
	_audioPaketSilence(0),
	_audioBufferPos(0),
	_audioBufferSize(0),
	_filename("")
{
	memset(&_audioPaket, 0, sizeof(_audioPaket));
	memset(&_audioPaketTemp, 0, sizeof(_audioPaketTemp));
}

FFmpegAudioDecodeStream* FFmpegAudioDecodeStream::open(const IPath &filename) {
	FFmpegAudioDecodeStream *stream = new FFmpegAudioDecodeStream();
	if (!stream->_open(filename)) {
		delete stream;
		return 0;
	}
	return stream;
}

bool FFmpegAudioDecodeStream::_open(const IPath &filename) {
	_filename = filename;
	if (!filename.isFile()) {
		logger.error("Audio-file does not exist: '" + filename.toNative() + "'", "UAudio_FFmpeg");
		return false;
	}

	// use custom 'ufile' protocol for UTF-8 support
	if (av_open_input_file(&_formatCtx,
			("ufile:" + filename.toUTF8()).c_str(), NULL, 0, NULL) != 0)
	{
		logger.error("av_open_input_file failed: '" + filename.toNative() + "'", "UAudio_FFmpeg");
		return false;
	}

	// generate PTS values if they do not exist
	_formatCtx->flags |= AVFMT_FLAG_GENPTS;

	// retrieve stream information
	if (av_find_stream_info(_formatCtx) < 0) {
		logger.error("av_find_stream_info failed: '" + filename.toNative() + "'", "UAudio_FFmpeg");
		return false;
	}

	// FIXME: hack used by ffplay. Maybe should not use url_feof() to test for the end
	_formatCtx->pb->eof_reached = 0;

#ifdef DEBUG_FFMPEG_DECODE
	dump_format(_formatCtx, 0, filename.toNative(), 0);
#endif

	_audioStreamIndex = ffmpegCore->findAudioStreamIndex(_formatCtx);
	if (_audioStreamIndex < 0) {
		logger.error("FindAudioStreamIndex: No Audio-stream found '" + filename.toNative() + "'", "UAudio_FFmpeg");
		return false;
	}

	//std::stringstream s;
	//s << _audioStreamIndex;
	//logger.status("AudioStreamIndex is: " + s.str(), "UAudio_FFmpeg");

	_audioStream = _formatCtx->streams[_audioStreamIndex];
	_audioStreamPos = 0;
	_codecCtx = _audioStream->codec;

#ifdef REQUEST_CHANNELS
	// TODO: should we use this or not? Should we allow 5.1 channel audio?
	#if LIBAVCODEC_VERSION_INT >= AV_VERSION_INT(51,42,0)
	if (_codecCtx->channels > 0)
		_codecCtx->request_channels = std::min(2, _codecCtx->channels);
	else
		_codecCtx->request_channels = 2;
	#endif
#endif

	_codec = avcodec_find_decoder(_codecCtx->codec_id);
	if (!_codec) {
		logger.error("Unsupported codec!", "UAudio_FFmpeg");
		_codecCtx = NULL;
		return false;
	}

	// set debug options
	_codecCtx->debug_mv = 0;
	_codecCtx->debug = 0;

	// detect bug-workarounds automatically
	_codecCtx->workaround_bugs = FF_BUG_AUTODETECT;
	// error resilience strategy (careful/compliant/agressive/very_aggressive)
	//CodecCtx->error_resilience = FF_ER_CAREFUL; //FF_ER_COMPLIANT;
	// allow non spec compliant speedup tricks.
	//CodecCtx->flags2 |= CODEC_FLAG2_FAST;

	// Note: avcodec_open() and avcodec_close() are not thread-safe and will
	// fail if called concurrently by different threads.
	int avResult;
	{
		MediaCore_FFmpeg::AVCodecLock codecLock;
		avResult = avcodec_open(_codecCtx, _codec);
	}
	if (avResult < 0) {
		logger.error("avcodec_open failed!", "UAudio_FFmpeg");
		return false;
	}

	// now initialize the audio-format

	audioSampleFormat_t sampleFormat;
	if (!ffmpegCore->convertFFmpegToAudioFormat(_codecCtx->sample_fmt, &sampleFormat)) {
		// try standard format
		sampleFormat = asfS16;
	}
	if (_codecCtx->channels > 255) {
		logger.status("Error: _codecCtx->channels > 255", "TFFmpegDecodeStream.Open");
	}
	_formatInfo = AudioFormatInfo(
		_codecCtx->channels,
		_codecCtx->sample_rate,
		sampleFormat
	);

	// finally start the decode thread
	start();

	return true;
}

void FFmpegAudioDecodeStream::close() {
	// wake threads waiting for packet-queue data
	// Note: normally, there are no waiting threads. If there were waiting
	// ones, they would block the audio-callback thread.
	_packetQueue.abort();

	// send quit request (to parse-thread etc)
	{
		Mutex::RegionLock lock(_stateLock);
		_quitRequest = true;
		_parserIdleCond.broadcast();
	}

	// abort parse-thread
	// and wait until it terminates
	wait();

	// Close the codec
	if (_codecCtx) {
		// avcodec_close() is not thread-safe
		MediaCore_FFmpeg::AVCodecLock codecLock;
		avcodec_close(_codecCtx);
	}

	// Close the video file
	if (_formatCtx) {
		av_close_input_file(_formatCtx);
	}
}

double FFmpegAudioDecodeStream::getLength() {
	// do not forget to consider the start_time value here
	// there is a type size mismatch warnign because start_time and duration are cint64.
	// So, in principle there could be an overflow when doing the sum.
	return (double)(_formatCtx->start_time + _formatCtx->duration) / AV_TIME_BASE;
}

double FFmpegAudioDecodeStream::getPosition() {
	DecoderPauser decoderPauser(this);

	// ReadData() does not return all of the buffer retrieved by DecodeFrame().
	// Determine the size of the unused part of the decode-buffer.
	double bufferSizeSec = (double)(_audioBufferSize - _audioBufferPos) /
		_formatInfo.getBytesPerSec();

	// subtract the size of unused buffer-data from the audio clock.
	return _audioStreamPos - bufferSizeSec;
}

void FFmpegAudioDecodeStream::setPosition(double time) {
	setPositionIntern(time, true, true);
}

/********************************************
* Parser section
********************************************/

void FFmpegAudioDecodeStream::setPositionIntern(double time, bool flush, bool blocking) {
	// - Pause the parser first to prevent it from putting obsolete packages
	//   into the queue after the queue was flushed and before seeking is done.
	//   Otherwise we will hear fragments of old data, if the stream was seeked
	//   in stopped mode and resumed afterwards (applies to non-blocking mode only).
	// - Pause the decoder to avoid race-condition that might occur otherwise.
	// - Last lock the state lock because we are manipulating some shared state-vars.
	{
		ParserPauser parserPauser(this);
		DecoderPauser decoderPauser(this);
		Mutex::RegionLock lock(_stateLock);

		_eofState = false;
		_errorState = false;

		// do not seek if we are already at the correct position.
		// This is important especially for seeking to position 0 if we already are
		// at the beginning. Although seeking with AVSEEK_FLAG_BACKWARD for pos 0 works,
		// it is still a bit choppy (although much better than w/o AVSEEK_FLAG_BACKWARD).
		if (time == _audioStreamPos)
			return;

		// configure seek parameters
		_seekPos = time;
		_seekFlush = flush;
		_seekFlags = AVSEEK_FLAG_ANY;
		_seekRequest = true;

		// Note: the BACKWARD-flag seeks to the first position <= the position
		// searched for. Otherwise e.g. position 0 might not be seeked correct.
		// For some reason ffmpeg sometimes doesn't use position 0 but the key-frame
		// following. In streams with few key-frames (like many flv-files) the next
		// key-frame after 0 might be 5secs ahead.
		if (time <= _audioStreamPos)
			_seekFlags |= AVSEEK_FLAG_BACKWARD;

		// send a reuse signal in case the parser was stopped (e.g. because of an EOF)
		_parserIdleCond.signal();
	}

	// in blocking mode, wait until seeking is done
	if (blocking) {
		Mutex::RegionLock lock(_stateLock);
		while (_seekRequest)
			_seekFinishedCond.wait(_stateLock);
	}
}

int FFmpegAudioDecodeStream::run() {
	// reuse thread as long as the stream is not terminated
	while (parseLoop()) {
		// wait for reuse or destruction of stream
		Mutex::RegionLock lock(_stateLock);
		while (!(_seekRequest || _quitRequest)) {
			_parserIdleCond.wait(_stateLock);
		}
	}
	return 0;
}

/**
* Parser main loop.
* Will not return until parsing of the stream is finished.
* Reasons for the parser to return are:
* - the end-of-file is reached
* - an error occured
* - the stream was quited (received a quit-request)
* Returns true if the stream can be resumed or false if the stream has to
* be terminated.
*/
bool FFmpegAudioDecodeStream::parseLoop() {
	AVPacket packet;
	while (true) {
		ParserLock parserLock(this);
		if (isQuit()) {
			return false;
		}

		// handle seek-request (Note: no need to lock SeekRequest here)
		if (_seekRequest) {
			// first try: seek on the audio stream
			int64_t seekTarget = llround(_seekPos / av_q2d(_audioStream->time_base));
			// duration of silence at start of stream
			double startSilence = 0;
			if (seekTarget < _audioStream->start_time)
				startSilence = (double)(_audioStream->start_time - seekTarget) * av_q2d(_audioStream->time_base);
			int errorCode = av_seek_frame(_formatCtx, _audioStreamIndex, seekTarget, _seekFlags);

			if (errorCode < 0) {
				// second try: seek on the default stream (necessary for flv-videos and some ogg-files)
				seekTarget = llround(_seekPos * AV_TIME_BASE);
				startSilence = 0;
				if (seekTarget < _formatCtx->start_time)
					startSilence = (double)(_formatCtx->start_time - seekTarget) / AV_TIME_BASE;
				errorCode = av_seek_frame(_formatCtx, -1, seekTarget, _seekFlags);
			}

			// pause decoder and lock state (keep the lock-order to avoid deadlocks).
			// Note that the decoder does not block in the packet-queue in seeking state,
			// so locking the decoder here does not cause a dead-lock.
			{
				DecoderPauser pause(this);
				Mutex::RegionLock lock(_stateLock);

				if (errorCode < 0) {
					// seeking failed
					_errorState = true;
					std::stringstream s;
					s << "Seek Error in '" << _formatCtx->filename << "'";
					logger.error(s.str(), "UAudioDecoder_FFmpeg");
				} else {
					if (_seekFlush) {
						// flush queue (we will send a Flush-Packet when seeking is finished)
						_packetQueue.flush();

						// flush the decode buffers
						_audioBufferSize = 0;
						_audioBufferPos = 0;
						_audioPaketSilence = 0;
						memset(&_audioPaketTemp, 0, sizeof(_audioPaketTemp));
						flushCodecBuffers();

						// Set preliminary stream position. The position will be set to
						// the correct value as soon as the first packet is decoded.
						_audioStreamPos = _seekPos;
					} else {
						// request avcodec buffer flush
						_packetQueue.putStatus(PKT_STATUS_FLAG_FLUSH, NULL);
					}

					// fill the gap between position 0 and start_time with silence
					// but not if we are in loop mode
					if (startSilence > 0 && !_loop) {
						// pointer for the EMPTY status packet
						double *startSilencePtr = (double*)malloc(sizeof(startSilence));
						*startSilencePtr = startSilence;
						_packetQueue.putStatus(PKT_STATUS_FLAG_EMPTY, startSilencePtr);
					}
				}
				_seekRequest = false;
				_seekFinishedCond.broadcast();
			}
		}

		if (_packetQueue.getSize() > MAX_AUDIOQ_SIZE) {
			Thread::sleep(10);
			continue;
		}

		if (av_read_frame(_formatCtx, &packet) < 0) {
			// failed to read a frame, check reason
			ByteIOContext *byteIOCtx;
#if LIBAVFORMAT_VERSION_MAJOR >= 52
			byteIOCtx = _formatCtx->pb;
#else
			byteIOCtx = &_formatCtx->pb;
#endif

			// check for end-of-file (eof is not an error)
			if (url_feof(byteIOCtx) != 0) {
				if (getLoop()) {
					// rewind stream (but do not flush)
					setPositionIntern(0, false, false);
					continue;
				} else {
					// signal end-of-file
					_packetQueue.putStatus(PKT_STATUS_FLAG_EOF, NULL);
					return true;
				}
			}

			// check for errors
			if (url_ferror(byteIOCtx) != 0) {
				// an error occured -> abort and wait for repositioning or termination
				_packetQueue.putStatus(PKT_STATUS_FLAG_ERROR, NULL);
				return true;
			}

			// url_feof() does not detect an EOF for some files
			// so we have to do it this way.
			if ((_formatCtx->file_size != 0) && (byteIOCtx->pos >= _formatCtx->file_size)) {
				_packetQueue.putStatus(PKT_STATUS_FLAG_EOF, NULL);
				return true;
			}

			// unknown error occured, exit
			_packetQueue.putStatus(PKT_STATUS_FLAG_ERROR, NULL);
			return true;
		}

		if (packet.stream_index == _audioStreamIndex) {
			_packetQueue.put(&packet);
		} else {
			av_free_packet(&packet);
		}
	}
}

/********************************************
* Decoder section
********************************************/

void FFmpegAudioDecodeStream::flushCodecBuffers() {
	// if no flush operation is specified, avcodec_flush_buffers will not do anything.
	if (_codecCtx->codec->flush) {
		// flush buffers used by avcodec_decode_audio, etc.
		avcodec_flush_buffers(_codecCtx);
	} else {
		// we need a Workaround to avoid plopping noise with ogg-vorbis and
		// mp3 (in older versions of FFmpeg).
		// We will just reopen the codec.
		MediaCore_FFmpeg::AVCodecLock codecLock;
		avcodec_close(_codecCtx);
		avcodec_open(_codecCtx, _codec);
	}
}

int FFmpegAudioDecodeStream::decodeFrame(uint8_t *buffer, int bufferSize) {
	if (isEOF())
		return -1;

	while (true) {
		// for titles with start_time > 0 we have to generate silence
		// until we reach the pts of the first data packet.
		if (_audioPaketSilence > 0) {
			int dataSize = std::min(_audioPaketSilence, bufferSize);
			memset(buffer, 0, dataSize);
			_audioPaketSilence -= dataSize;
			_audioStreamPos += dataSize / _formatInfo.getBytesPerSec();
			return dataSize;
		}

		// read packet data
		while (_audioPaketTemp.size > 0) {
			// size of output data decoded by FFmpeg
			int dataSize = bufferSize;

			int paketDecodedSize; // size of packet data used for decoding
			paketDecodedSize = avcodec_decode_audio3(_codecCtx, (int16_t*)buffer,
				&dataSize, &_audioPaketTemp);

			if (paketDecodedSize < 0) {
				// if error, skip frame
#ifdef DEBUG_FFMPEG_DECODE
				logger.status("Skip audio frame", "");
#endif
				_audioPaketTemp.size = 0;
				break;
			}

			_audioPaketTemp.data += paketDecodedSize;
			_audioPaketTemp.size -= paketDecodedSize;

			// check if avcodec_decode_audio returned data, otherwise fetch more frames
			if (dataSize <= 0)
				continue;

			// update stream position by the amount of fetched data
			_audioStreamPos += dataSize / _formatInfo.getBytesPerSec();

			// we have data, return it and come back for more later
			return dataSize;
		}

		// free old packet data
		if (_audioPaket.data)
			av_free_packet(&_audioPaket);

		// do not block queue on seeking (to avoid deadlocks on the DecoderLock)
		bool blockQueue = !isSeeking();

		// request a new packet and block if none available.
		// If this fails, the queue was aborted.
		if (_packetQueue.get(&_audioPaket, blockQueue) <= 0)
			return -1;

		// handle Status-packet
		if (_audioPaket.data == STATUS_PACKET) {
			_audioPaket.data = NULL;
			memset(&_audioPaketTemp, 0, sizeof(_audioPaketTemp));

			switch (_audioPaket.flags) {
			case PKT_STATUS_FLAG_FLUSH:
				// just used if SetPositionIntern was called without the flush flag.
				flushCodecBuffers();
				break;
			case PKT_STATUS_FLAG_EOF: // end-of-file
				// ignore EOF while seeking
				if (!isSeeking())
					setEOF(true);
				// buffer contains no data
				return -1;
			case PKT_STATUS_FLAG_ERROR:
				setError(true);
				logger.status("I/O Error", "TFFmpegDecodeStream.DecodeFrame");
				return -1;
			case PKT_STATUS_FLAG_EMPTY: {
				double silenceDuration = *(double*) (_packetQueue.getStatusInfo(&_audioPaket));
				_audioPaketSilence = lround(silenceDuration * _formatInfo.getSampleRate()) *
						_formatInfo.getFrameSize();
				_packetQueue.freeStatusInfo(&_audioPaket);
				break;
			}
			default:
				logger.status("Unknown status", "TFFmpegDecodeStream.DecodeFrame");
			}

			continue;
		}

		_audioPaketTemp.data = _audioPaket.data;
		_audioPaketTemp.size = _audioPaket.size;

		// if available, update the stream position to the presentation time of this package
		if (_audioPaket.pts != (int64_t)AV_NOPTS_VALUE) {
#ifdef DEBUG_FFMPEG_DECODE
			double tmpPos = _audioStreamPos;
#endif
			_audioStreamPos = av_q2d(_audioStream->time_base) * _audioPaket.pts;
#ifdef DEBUG_FFMPEG_DECODE
			stringstream s;
			s << "Timestamp: " << _audioStreamPos << " "
			  << "(Calc: " << tmpPos << "), "
			  << "Diff: "  << (_audioStreamPos-TmpPos);
			logger.status(s.str(), "");
#endif
		}
	}
}

int FFmpegAudioDecodeStream::readData(uint8_t *buffer, int bufferSize) {
	// set number of bytes to copy to the output buffer
	int bufferPos = 0;

	{
		DecoderLock decoderLock(this);

		// leave if end-of-file is reached
		if (isEOF()) {
			return -1;
		}

		// copy data to output buffer
		while (bufferPos < bufferSize) {
			// check if we need more data
			if (_audioBufferPos >= _audioBufferSize) {
				_audioBufferPos = 0;

				// we have already sent all our data; get more
				_audioBufferSize = decodeFrame(_audioBuffer, AUDIO_BUFFER_SIZE);
				if (_audioBufferSize < 0) {
					// error or EOF occurred
					return bufferPos;
				}
			}

			// calc number of new bytes in the decode-buffer (number of bytes to copy)
			int copyByteCount = _audioBufferSize - _audioBufferPos;
			// number of bytes left (remain) to read
			int remainByteCount = bufferSize - bufferPos;
			// resize copy-count if more bytes available than needed (remaining bytes are used the next time)
			if (copyByteCount > remainByteCount)
				copyByteCount = remainByteCount;

			memcpy(&buffer[bufferPos], &_audioBuffer[_audioBufferPos], copyByteCount);
			bufferPos += copyByteCount;
			_audioBufferPos += copyByteCount;
		}
	}
	return bufferSize;
}

/************************************
 * C Interface
 ************************************/

#define DecodeStreamObj(ptr) reinterpret_cast<PluginDecodeStream*>(ptr)

static BOOL PLUGIN_CALL ffmpegAudioDecoder_init() {
	return TRUE;
}

static BOOL PLUGIN_CALL ffmpegAudioDecoder_finalize() {
	return TRUE;
}

static audioDecodeStream_t* PLUGIN_CALL ffmpegAudioDecoder_open(const char *filename) {
	return (audioDecodeStream_t*)FFmpegAudioDecodeStream::open(filename);
}

static void PLUGIN_CALL ffmpegAudioDecoder_close(audioDecodeStream_t *stream) {
	delete DecodeStreamObj(stream);
}

static double PLUGIN_CALL ffmpegAudioDecoder_getLength(audioDecodeStream_t *stream) {
	return DecodeStreamObj(stream)->getLength();
}

static void PLUGIN_CALL ffmpegAudioDecoder_getAudioFormatInfo(audioDecodeStream_t *stream, audioFormatInfo_t *info) {
	DecodeStreamObj(stream)->getAudioFormatInfo().toCStruct(info);
}

static double PLUGIN_CALL ffmpegAudioDecoder_getPosition(audioDecodeStream_t *stream) {
	return DecodeStreamObj(stream)->getPosition();
}

static void PLUGIN_CALL ffmpegAudioDecoder_setPosition(audioDecodeStream_t *stream, double time) {
	DecodeStreamObj(stream)->setPosition(time);
}

static BOOL PLUGIN_CALL ffmpegAudioDecoder_getLoop(audioDecodeStream_t *stream) {
	return (BOOL)DecodeStreamObj(stream)->getLoop();
}

static void PLUGIN_CALL ffmpegAudioDecoder_setLoop(audioDecodeStream_t *stream, BOOL enabled) {
	DecodeStreamObj(stream)->setLoop(enabled);
}

static BOOL PLUGIN_CALL ffmpegAudioDecoder_isEOF(audioDecodeStream_t *stream) {
	return (BOOL)DecodeStreamObj(stream)->isEOF();
}

static BOOL PLUGIN_CALL ffmpegAudioDecoder_isError(audioDecodeStream_t *stream) {
	return (BOOL)DecodeStreamObj(stream)->isError();
}

static int PLUGIN_CALL ffmpegAudioDecoder_readData(audioDecodeStream_t *stream, uint8_t *buffer, int bufferSize) {
	return DecodeStreamObj(stream)->readData(buffer, bufferSize);
}

/************************************
 * Module information
 ************************************/

const audioDecoderInfo_t audioDecoderInfo = {
		50,
		ffmpegAudioDecoder_init,
		ffmpegAudioDecoder_finalize,
		ffmpegAudioDecoder_open,
		ffmpegAudioDecoder_close,
		ffmpegAudioDecoder_getLength,
		ffmpegAudioDecoder_getAudioFormatInfo,
		ffmpegAudioDecoder_getPosition,
		ffmpegAudioDecoder_setPosition,
		ffmpegAudioDecoder_getLoop,
		ffmpegAudioDecoder_setLoop,
		ffmpegAudioDecoder_isEOF,
		ffmpegAudioDecoder_isError,
		ffmpegAudioDecoder_readData
};
