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
#ifndef _FFMPEG_AUDIO_DECODE_H_
#define _FFMPEG_AUDIO_DECODE_H_

#include "ffmpeg_core.h"
#include "core/plugin_audio_decode.h"

// TODO: The factor 3/2 might not be necessary as we do not need extra
// space for synchronizing as in the tutorial.
#define AUDIO_BUFFER_SIZE ((AVCODEC_MAX_AUDIO_FRAME_SIZE * 3) / 2)

extern const audioDecoderInfo_t audioDecoderInfo;

class FFmpegAudioDecodeStream : public PluginDecodeStream, private Thread {
private:
	Mutex _stateLock;

	bool _eofState; // end-of-stream flag (locked by StateLock)
	bool _errorState; // error flag (locked by StateLock)

	bool _quitRequest; // (locked by StateLock)
	Condition _parserIdleCond;

	// parser pause/resume data
	bool _parserLocked;
	int _parserPauseRequestCount;
	Condition _parserUnlockedCond;
	Condition _parserResumeCond;

	bool _seekRequest; // (locked by StateLock)
	int _seekFlags; // (locked by StateLock)
	double _seekPos;    // stream position to seek for (in secs) (locked by StateLock)
	bool _seekFlush;   // true if the buffers should be flushed after seeking (locked by StateLock)
	Condition _seekFinishedCond;

	bool _loop; // (locked by StateLock)

	PacketQueue _packetQueue;

	AudioFormatInfo _formatInfo;

	// FFmpeg specific data
	AVFormatContext *_formatCtx;
	AVCodecContext *_codecCtx;
	AVCodec *_codec;

	int _audioStreamIndex;
	AVStream *_audioStream;
	double _audioStreamPos; // stream position in seconds (locked by DecoderLock)

	// decoder pause/resume data
	bool _decoderLocked;
	int _decoderPauseRequestCount;
	Condition _decoderUnlockedCond;
	Condition _decoderResumeCond;

	// state-vars for DecodeFrame (locked by DecoderLock)
	AVPacket _audioPaket;
	AVPacket _audioPaketTemp;
	int _audioPaketSilence; // number of bytes of silence to return

	// state-vars for AudioCallback (locked by DecoderLock)
	int _audioBufferPos;
	int _audioBufferSize;
	DECLARE_ALIGNED(16, uint8_t, _audioBuffer[AUDIO_BUFFER_SIZE]);

	IPath _filename;

private:
	FFmpegAudioDecodeStream();

	void setPositionIntern(double time, bool flush, bool blocking);

	void setEOF(bool state) {
		Mutex::RegionLock lock(_stateLock);
		_eofState = state;
	}

	void setError(bool state) {
		Mutex::RegionLock lock(_stateLock);
		_errorState = state;
	}

	bool isSeeking() {
		Mutex::RegionLock lock(_stateLock);
		return _seekRequest;
	}

	bool isQuit() {
		Mutex::RegionLock lock(_stateLock);
		return _quitRequest;
	}

	bool parseLoop();

	int decodeFrame(uint8_t *buffer, int bufferSize);
	void flushCodecBuffers();

	bool _open(const IPath &filename);
	void close();

public:
	virtual ~FFmpegAudioDecodeStream() {
		close();
	}

	static FFmpegAudioDecodeStream* open(const IPath &filename);

	virtual double getLength();

	virtual const AudioFormatInfo &getAudioFormatInfo() {
		return _formatInfo;
	}

	virtual double getPosition();
	virtual void setPosition(double time);

	virtual bool getLoop() {
		Mutex::RegionLock lock(_stateLock);
		return _loop;
	}

	virtual void setLoop(bool Enabled) {
		Mutex::RegionLock lock(_stateLock);
		_loop = Enabled;
	}

	virtual bool isEOF() {
		Mutex::RegionLock lock(_stateLock);
		return _eofState;
	}

	virtual bool isError() {
		Mutex::RegionLock lock(_stateLock);
		return _errorState;
	}

	virtual int readData(uint8_t *buffer, int bufferSize);

public:
	int run();

public:
	class ParserLock {
	private:
		FFmpegAudioDecodeStream *_stream;
	public:
		// Note: pthreads wakes threads waiting on a mutex in the order of their
		// priority and not in FIFO order. SDL does not provide any option to
		// control priorities. This might (and already did) starve threads waiting
		// on the mutex (e.g. SetPosition) making usdx look like it was froozen.
		// Instead of simply locking the critical section we set a ParserLocked flag
		// instead and give priority to the threads requesting the parser to pause.
		ParserLock(FFmpegAudioDecodeStream *stream) : _stream(stream) {
			Mutex::RegionLock lock(_stream->_stateLock);
			while (_stream->_parserPauseRequestCount > 0)
				_stream->_parserResumeCond.wait(_stream->_stateLock);
			_stream->_parserLocked = true;
		}

		~ParserLock() {
			Mutex::RegionLock lock(_stream->_stateLock);
			_stream->_parserLocked = false;
			_stream->_parserUnlockedCond.broadcast();
		}
	};

	class ParserPauser {
	private:
		FFmpegAudioDecodeStream *_stream;
	public:
		ParserPauser(FFmpegAudioDecodeStream *stream) : _stream(stream) {
			if (Thread::getCurrentThreadID() == _stream->getThreadID())
				return;

			{
				Mutex::RegionLock lock(_stream->_stateLock);
				++_stream->_parserPauseRequestCount;
				while (_stream->_parserLocked)
					_stream->_parserUnlockedCond.wait(_stream->_stateLock);
			}
		}

		~ParserPauser() {
			if (Thread::getCurrentThreadID() == _stream->getThreadID())
				return;

			{
				Mutex::RegionLock lock(_stream->_stateLock);
				--_stream->_parserPauseRequestCount;
				_stream->_parserResumeCond.signal();
			}
		}
	};

	class DecoderLock {
	private:
		FFmpegAudioDecodeStream *_stream;
	public:
		// prioritize pause requests
		DecoderLock(FFmpegAudioDecodeStream *stream) : _stream(stream) {
			Mutex::RegionLock lock(_stream->_stateLock);
			while (_stream->_decoderPauseRequestCount > 0)
				_stream->_decoderResumeCond.wait(_stream->_stateLock);
			_stream->_decoderLocked = true;
		}

		~DecoderLock() {
			Mutex::RegionLock lock(_stream->_stateLock);
			_stream->_decoderLocked = false;
			_stream->_decoderUnlockedCond.broadcast();
		}
	};

	class DecoderPauser {
	private:
		FFmpegAudioDecodeStream *_stream;
	public:
		DecoderPauser(FFmpegAudioDecodeStream *stream) : _stream(stream) {
			Mutex::RegionLock lock(_stream->_stateLock);
			++_stream->_decoderPauseRequestCount;
			while (_stream->_decoderLocked)
				_stream->_decoderUnlockedCond.wait(_stream->_stateLock);
		}

		~DecoderPauser() {
			Mutex::RegionLock lock(_stream->_stateLock);
			--_stream->_decoderPauseRequestCount;
			_stream->_decoderResumeCond.signal();
		}
	};
};

#endif /* _FFMPEG_AUDIO_DECODE_H_ */
