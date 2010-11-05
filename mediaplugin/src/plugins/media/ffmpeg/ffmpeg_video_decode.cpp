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

/*
 * based on 'An ffmpeg and SDL Tutorial' (http://www.dranger.com/ffmpeg/)
 */

#include "ffmpeg_video_decode.h"
#include <sstream>

// These are called whenever we allocate a frame buffer.
// We use this to store the global_pts in a frame at the time it is allocated.
int CDECL ptsGetBuffer(AVCodecContext *codecCtx, AVFrame *frame) {
	int result = avcodec_default_get_buffer(codecCtx, frame);
	int64_t *videoPktPts = (int64_t*)codecCtx->opaque;
	if (videoPktPts) {
		// Note: we must copy the pts instead of passing a pointer, because the packet
		// (and with it the pts) might change before a frame is returned by av_decode_video.
		int64_t *pts = (int64_t*)av_malloc(sizeof(int64_t));
		*pts = *videoPktPts;
		frame->opaque = pts;
	}
	return result;
}

void CDECL ptsReleaseBuffer(AVCodecContext *codecCtx, AVFrame *frame) {
	if (frame)
		av_freep(&frame->opaque);
	avcodec_default_release_buffer(codecCtx, frame);
}

/*
 * TVideoDecoder_FFmpeg
 */

FFmpegVideoDecodeStream::FFmpegVideoDecodeStream() :
		_opened(false),
		_eof(false),
		_loop(false),
		_stream(NULL),
		_streamIndex(-1),
		_formatContext(NULL),
		_codecContext(NULL),
		_codec(NULL),
		_avFrame(NULL),
		_avFrameRGB(NULL),
		_frameBuffer(NULL),
		_frameTexValid(false),
#ifdef USE_SWSCALE
		_swScaleContext(NULL),
#endif
		_aspect(0),
		_frameDuration(0),
		_frameTime(0),
		_loopTime(0) {}

FFmpegVideoDecodeStream* FFmpegVideoDecodeStream::open(const IPath &filename) {
	FFmpegVideoDecodeStream *stream = new FFmpegVideoDecodeStream();
	if (!stream->_open(filename)) {
		delete stream;
		return 0;
	}
	return stream;
}

bool FFmpegVideoDecodeStream::_open(const IPath &filename) {
	std::stringstream ss;

	// use custom 'ufile' protocol for UTF-8 support
	int errnum = av_open_input_file(&_formatContext,
			("ufile:" + filename.toUTF8()).c_str(), NULL, 0, NULL);
	if (errnum != 0) {
		logger.error("Failed to open file '" + filename.toNative() + "' ("
				+ ffmpegCore->getErrorString(errnum) + ")",
				"VideoDecodeStream_FFmpeg::Open");
		return false;
	}

	// update video info
	if (av_find_stream_info(_formatContext) < 0) {
		logger.error("No stream info found", "VideoPlayback_ffmpeg.Open");
		close();
		return false;
	}

	ss.str("");
	ss << "VideoStreamIndex: " << _streamIndex;
	logger.info(ss.str(), "VideoPlayback_ffmpeg.Open");

	// find video stream
	int audioStreamIndex;
	ffmpegCore->findStreamIDs(_formatContext, &_streamIndex, &audioStreamIndex);
	if (_streamIndex < 0) {
		logger.error("No video stream found", "VideoPlayback_ffmpeg.Open");
		close();
		return false;
	}

	_stream = _formatContext->streams[_streamIndex];
	_codecContext = _stream->codec;

	_codec = avcodec_find_decoder(_codecContext->codec_id);
	if (!_codec) {
		logger.error("No matching codec found", "VideoPlayback_ffmpeg.Open");
		close();
		return false;
	}

	// set debug options
	_codecContext->debug_mv = 0;
	_codecContext->debug = 0;

	// detect bug-workarounds automatically
	_codecContext->workaround_bugs = FF_BUG_AUTODETECT;
	// error resilience strategy (careful/compliant/agressive/very_aggressive)
	//fCodecContext->error_resilience = FF_ER_CAREFUL; //FF_ER_COMPLIANT;
	// allow non spec compliant speedup tricks.
	//fCodecContext->flags2 = fCodecContext->flags2 | CODEC_FLAG2_FAST;

	// Note: avcodec_open() and avcodec_close() are not thread-safe and will
	// fail if called concurrently by different threads.
	{
		MediaCore_FFmpeg::AVCodecLock codecLock;
		errnum = avcodec_open(_codecContext, _codec);
	}
	if (errnum < 0) {
		logger.error("No matching codec found", "VideoPlayback_ffmpeg.Open");
		close();
		return false;
	}

	// register custom callbacks for pts-determination
	_codecContext->get_buffer = ptsGetBuffer;
	_codecContext->release_buffer = ptsReleaseBuffer;

#ifdef DEBUG_DISPLAY
	ss.str("");
	ss << "Found a matching Codec: " << _codecContext->codec->name << std::endl
	   << std::endl
	   << "  Width = "     << _codecContext->width
	   << ", Height="      << _codecContext->height << std::endl
	   << "  Aspect : "    << _codecContext->sample_aspect_ratio.num << "/"
	                       << _codecContext->sample_aspect_ratio.den << std::endl
	   << "  Framerate : " << _codecContext->time_base.num << "/"
	                       << _codecContext->time_base.den;
	logger.status(ss.str(), "");
#endif

	// allocate space for decoded frame and rgb frame
	_avFrame = avcodec_alloc_frame();
	_avFrameRGB = avcodec_alloc_frame();
	_frameBuffer = (uint8_t*) av_malloc(avpicture_get_size(PIXEL_FMT_FFMPEG,
			_codecContext->width, _codecContext->height));

	if (!_avFrame || !_avFrameRGB || !_frameBuffer) {
		logger.error("Failed to allocate buffers", "VideoPlayback_ffmpeg.Open");
		close();
		return false;
	}

	// TODO: pad data for OpenGL to GL_UNPACK_ALIGNMENT
	// (otherwise video will be distorted if width/height is not a multiple of the alignment)
	errnum = avpicture_fill((AVPicture*)_avFrameRGB, _frameBuffer,
			PIXEL_FMT_FFMPEG, _codecContext->width, _codecContext->height);
	if (errnum < 0) {
		logger.error("avpicture_fill failed: " + ffmpegCore->getErrorString(errnum),
				"VideoPlayback_ffmpeg.Open");
		close();
		return false;
	}

	// calculate some information for video display
	_aspect = av_q2d(_codecContext->sample_aspect_ratio);
	if (_aspect == 0) {
		_aspect = (double)_codecContext->width / _codecContext->height;
	} else {
		_aspect *= (double)_codecContext->width / _codecContext->height;
	}

	_frameDuration = 1.0 / av_q2d(_stream->r_frame_rate);

	// hack to get reasonable framerate (for divx and others)
	if (_frameDuration < 0.02) { // 0.02 <-> 50 fps
		_frameDuration = av_q2d(_stream->r_frame_rate);
		while (_frameDuration > 50.0)
			_frameDuration /= 10.0;
		_frameDuration = 1.0 / _frameDuration;
	}

	ss.str("");
	ss << "Framerate: " << (int)(1.0 / _frameDuration) << "fps";
	logger.info(ss.str(), "VideoPlayback_ffmpeg.Open");

#ifdef USE_SWSCALE
	// if available get a SWScale-context -> faster than the deprecated img_convert().
	// SWScale has accelerated support for PIX_FMT_RGB32/PIX_FMT_BGR24/PIX_FMT_BGR565/PIX_FMT_BGR555.
	// Note: PIX_FMT_RGB32 is a BGR- and not an RGB-format (maybe a bug)!!!
	// The BGR565-formats (GL_UNSIGNED_SHORT_5_6_5) is way too slow because of its
	// bad OpenGL support. The BGR formats have MMX(2) implementations but no speed-up
	// could be observed in comparison to the RGB versions.
	_swScaleContext = sws_getCachedContext(NULL,
			_codecContext->width, _codecContext->height, _codecContext->pix_fmt,
			_codecContext->width, _codecContext->height, PIXEL_FMT_FFMPEG,
			SWS_FAST_BILINEAR,
			NULL, NULL, NULL);
	if (!_swScaleContext) {
		logger.error("Failed to get swscale context", "VideoPlayback_ffmpeg.Open");
		close();
		return false;
	}
#endif

	_opened = true;
	return true;
}

void FFmpegVideoDecodeStream::close() {
	if (_frameBuffer)
		av_free(_frameBuffer);
	if (_avFrameRGB)
		av_free(_avFrameRGB);
	if (_avFrame)
		av_free(_avFrame);

	_avFrame = NULL;
	_avFrameRGB = NULL;
	_frameBuffer = NULL;

	if (_codecContext) {
		// avcodec_close() is not thread-safe
		MediaCore_FFmpeg::AVCodecLock codecLock;
		avcodec_close(_codecContext);
	}

	if (_formatContext)
		av_close_input_file(_formatContext);

	_codecContext = NULL;
	_formatContext = NULL;

	_opened = false;
}

void FFmpegVideoDecodeStream::synchronizeTime(AVFrame *frame, double &pts) {
	if (pts != 0) {
		// if we have pts, set video clock to it
		_frameTime = pts;
	} else {
		// if we aren't given a pts, set it to the clock
		pts = _frameTime;
	}
	// update the video clock
	double frameDelay = av_q2d(_codecContext->time_base);
	// if we are repeating a frame, adjust clock accordingly
	frameDelay = frameDelay + frame->repeat_pict * (frameDelay * 0.5);
	_frameTime = _frameTime + frameDelay;
}

/**
 * Decode a new frame from the video stream.
 * The decoded frame is stored in fAVFrame. fFrameTime is updated to the new frame's
 * time.
 * @param pts will be updated to the presentation time of the decoded frame.
 * returns true if a frame could be decoded. False if an error or EOF occured.
 */
bool FFmpegVideoDecodeStream::decodeFrame() {
	int64_t videoPktPts;
	ByteIOContext *pbIOCtx;
	int errnum;
	AVPacket packet;
	double pts;

	if (_eof)
		return false;

	// read packets until we have a finished frame (or there are no more packets)
	int frameFinished = 0;
	while (frameFinished == 0) {
		errnum = av_read_frame(_formatContext, &packet);
		if (errnum < 0) {
			// failed to read a frame, check reason

#if LIBAVFORMAT_VERSION_MAJOR >= 52
			pbIOCtx = _formatContext->pb;
#else
			pbIOCtx = &_formatContext->pb;
#endif

			// check for end-of-file (EOF is not an error)
			if (url_feof(pbIOCtx) != 0) {
				_eof = true;
				return false;
			}

			// check for errors
			if (url_ferror(pbIOCtx) != 0) {
				logger.error("Video decoding file error", "TVideoPlayback_FFmpeg.DecodeFrame");
				return false;
			}

			// url_feof() does not detect an EOF for some mov-files (e.g. deluxe.mov)
			// so we have to do it this way.
			if ((_formatContext->file_size != 0) &&
					(pbIOCtx->pos >= _formatContext->file_size))
			{
				_eof = true;
				return false;
			}

			// error occured, log and exit
			logger.error("Video decoding error", "TVideoPlayback_FFmpeg.DecodeFrame");
			return false;
		}

		// if we got a packet from the video stream, then decode it
		if (packet.stream_index == _streamIndex) {
			// save pts to be stored in pFrame in first call of PtsGetBuffer()
			videoPktPts = packet.pts;
			// FIXME: is the pointer valid when it is used?
			_codecContext->opaque = &videoPktPts;

			// decode packet
			avcodec_decode_video2(_codecContext, _avFrame, &frameFinished, &packet);

			// reset opaque data
			_codecContext->opaque = NULL;

			// update pts
			if (packet.dts != (int64_t)AV_NOPTS_VALUE) {
				pts = packet.dts;
			} else if (_avFrame->opaque &&
					(*((int64_t*)_avFrame->opaque) != (int64_t)AV_NOPTS_VALUE))
			{
				pts = *((int64_t*)_avFrame->opaque);
			} else {
				pts = 0;
			}

			if (_stream->start_time != (int64_t)AV_NOPTS_VALUE)
				pts -= _stream->start_time;

			pts *= av_q2d(_stream->time_base);

			// synchronize time on each complete frame
			if (frameFinished != 0)
				synchronizeTime(_avFrame, pts);
		}

		// free the packet from av_read_frame
		av_free_packet(&packet);
	}

	return true;
}

#ifdef DEBUG_FRAMES
void spawnGoldenRec(double x, double y, int screen, uint8_t live, int startFrame,
		int recArrayIndex, unsigned player)
{
	//GoldenRec.Spawn(x, y, screen, live, startFrame, recArrayIndex, ColoredStar, player)
}
#endif

uint8_t* FFmpegVideoDecodeStream::getFrame(long double time) {
	const long double SKIP_FRAME_DIFF = 0.010; // start skipping if we are >= 10ms too late
	std::stringstream ss;

	if (!_opened)
		return NULL;

	/*
	 * Synchronization - begin
	 */

	// requested stream position (relative to the last loop's start)
	long double currentTime;
	if (_loop)
		currentTime = time - _loopTime;
	else
		currentTime = time;

	// check if current texture still contains the active frame
	if (_frameTexValid) {
		// time since the last frame was returned
		long double timeDiff = currentTime - _frameTime;

#ifdef DEBUG_DISPLAY
		ss.str("");
		ss << "time: " << floor(time*1000) << std::endl
		   << "VideoTime: " << floor(_frameTime*1000) << std::endl
		   << "TimeBase: " << floor(_frameDuration*1000) << std::endl
		   << "timeDiff: " << floor(timeDiff*1000);
		logger.status(ss.str(), "");
#endif

		// check if time has reached the next frame
		if (timeDiff < _frameDuration) {
#ifdef DEBUG_FRAMES
			// frame delay debug display
			spawnGoldenRec(200, 15, 1, 16, 0, -1, 0x00ff00);
#endif

#ifdef DEBUG_DISPLAY
			ss.str("");
			ss << "not getting new frame" << std::endl
			   << "time: " << floor(time*1000) << std::endl
			   << "VideoTime: " << floor(_frameTime*1000) << std::endl
			   << "TimeBase: " << floor(_frameDuration*1000) << std::endl
			   << "timeDiff: " << floor(timeDiff*1000);
			logger.status(ss.str(), "");
#endif

			// we do not need a new frame now
			return NULL;
		}
	}

	// fetch new frame (updates fFrameTime)
	bool success = decodeFrame();
	long double timeDiff = currentTime - _frameTime;

	// check if we have to skip frames
	// Either if we are one frame behind or if the skip threshold has been reached.
	// Do not skip if the difference is less than fFrameDuration as there is no next frame.
	// Note: We assume that fFrameDuration is the length of one frame.
	if (timeDiff >= std::max(_frameDuration, SKIP_FRAME_DIFF)) {
#ifdef DEBUG_FRAMES
		//frame drop debug display
		spawnGoldenRec(200, 55, 1, 16, 0, -1, 0xff0000);
#endif
#ifdef DEBUG_DISPLAY
		ss.str("");
		ss << "skipping frames" << std::endl
		   << "TimeBase: " << floor(_frameDuration*1000) << std::endl
		   << "timeDiff: " << floor(timeDiff*1000);
		logger.status(ss.str(), "");
#endif

		// update video-time
		int dropFrameCount = (int)(timeDiff / _frameDuration);
		_frameTime = _frameTime + dropFrameCount * _frameDuration;

		// skip frames
		for (int i = 1; i <= dropFrameCount; ++i)
			success = decodeFrame();
	}

	// check if we got an EOF or error
	if (!success) {
		if (_loop) {
			// we have to loop, so rewind
			setPosition(0);
			// record the start-time of the current loop, so we can
			// determine the position in the stream (fFrameTime-fLoopTime) later.
			_loopTime = time;
		}
		return NULL;
	}

	/*
	 * Synchronization - end
	 */

	// TODO: support for pan&scan
	//if (_avFrame->pan_scan) {
	//	printf("PanScan: %d/%d", _avFrame->pan_scan->width, _avFrame->pan_scan->height);
	//}

	// otherwise we convert the pixeldata from YUV to RGB
	int errnum;
#ifdef USE_SWSCALE
	errnum = sws_scale(_swScaleContext,
			(uint8_t**)_avFrame->data, _avFrame->linesize,
			0, _codecContext->height,
			(uint8_t**)_avFrameRGB->data, _avFrameRGB->linesize);
#else
	// img_convert from lib/ffmpeg/avcodec.pas is actually deprecated.
	// If ./configure does not find SWScale then this gives the error
	// that the identifier img_convert is not known or similar.
	// I think this should be removed, but am not sure whether there should
	// be some other replacement or a warning, Therefore, I leave it for now.
	// April 2009, mischi
	errnum = img_convert((AVPicture*)_avFrameRGB, PIXEL_FMT_FFMPEG,
			(AVPicture*)_avFrame, _codecContext->pix_fmt,
			_codecContext->width, _codecContext->height);
#endif

	if (errnum < 0) {
		logger.error("Image conversion failed", "TVideoPlayback_ffmpeg.GetFrame");
		return NULL;
	}

	if (!_frameTexValid)
		_frameTexValid = true;

	return _avFrameRGB->data[0];
}

void FFmpegVideoDecodeStream::setLoop(bool enable) {
	_loop = enable;
	_loopTime = 0;
}

bool FFmpegVideoDecodeStream::getLoop() {
	return _loop;
}

/**
 * Sets the stream's position.
 * The stream is set to the first keyframe with timestamp <= Time.
 * Note that fFrameTime is set to Time no matter if the actual position seeked to is
 * at Time or the time of a preceding keyframe. fFrameTime will be updated to the
 * actual frame time when GetFrame() is called the next time.
 * @param Time new position in seconds
 */
void FFmpegVideoDecodeStream::setPosition(double time) {
	int seekFlags;

	if (!_opened)
		return;

	if (time < 0)
		time = 0;

	// TODO: handle fLoop-times
	//time %= videoDuration;

	// Do not use the AVSEEK_FLAG_ANY here. It will seek to any frame, even
	// non keyframes (P-/B-frames). It will produce corrupted video frames as
	// FFmpeg does not use the information of the preceding I-frame.
	// The picture might be gray or green until the next keyframe occurs.
	// Instead seek the first keyframe smaller than the requested time
	// (AVSEEK_FLAG_BACKWARD). As this can be some seconds earlier than the
	// requested time, let the sync in GetFrame() do its job.
	seekFlags = AVSEEK_FLAG_BACKWARD;

	_frameTime = time;
	_eof = false;
	_frameTexValid = false;

	if (av_seek_frame(_formatContext, _streamIndex,
			llround(time / av_q2d(_stream->time_base)), seekFlags) < 0)
	{
		logger.error("av_seek_frame() failed", "TVideoPlayback_ffmpeg.SetPosition");
		return;
	}

	avcodec_flush_buffers(_codecContext);
}

double FFmpegVideoDecodeStream::getPosition() {
	return _frameTime;
}

int FFmpegVideoDecodeStream::getFrameWidth() {
	return _codecContext->width;
}

int FFmpegVideoDecodeStream::getFrameHeight() {
	return _codecContext->height;
}

double FFmpegVideoDecodeStream::getFrameAspect() {
	return _aspect;
}

/************************************
 * C Interface
 ************************************/

#define VideoDecodeStreamObj(ptr) reinterpret_cast<FFmpegVideoDecodeStream*>(ptr)

static BOOL PLUGIN_CALL ffmpegVideoDecoder_init() {
	return TRUE;
}

static BOOL PLUGIN_CALL ffmpegVideoDecoder_finalize() {
	return TRUE;
}

static videoDecodeStream_t* PLUGIN_CALL ffmpegVideoDecoder_open(const char *filename) {
	return (videoDecodeStream_t*)FFmpegVideoDecodeStream::open(filename);
}

static void PLUGIN_CALL ffmpegVideoDecoder_close(videoDecodeStream_t *stream) {
	delete VideoDecodeStreamObj(stream);
}

static void PLUGIN_CALL ffmpegVideoDecoder_setLoop(videoDecodeStream_t *stream, BOOL enable) {
	VideoDecodeStreamObj(stream)->setLoop(enable);
}

static BOOL PLUGIN_CALL ffmpegVideoDecoder_getLoop(videoDecodeStream_t *stream) {
	return (BOOL)VideoDecodeStreamObj(stream)->getLoop();
}

static void PLUGIN_CALL ffmpegVideoDecoder_setPosition(videoDecodeStream_t *stream, double time) {
	VideoDecodeStreamObj(stream)->setPosition(time);
}

static double PLUGIN_CALL ffmpegVideoDecoder_getPosition(videoDecodeStream_t *stream) {
	return VideoDecodeStreamObj(stream)->getPosition();
}

static int PLUGIN_CALL ffmpegVideoDecoder_getFrameWidth(videoDecodeStream_t *stream) {
	return VideoDecodeStreamObj(stream)->getFrameWidth();
}

static int PLUGIN_CALL ffmpegVideoDecoder_getFrameHeight(videoDecodeStream_t *stream) {
	return VideoDecodeStreamObj(stream)->getFrameHeight();
}

static double PLUGIN_CALL ffmpegVideoDecoder_getFrameAspect(videoDecodeStream_t *stream) {
	return VideoDecodeStreamObj(stream)->getFrameAspect();
}

static uint8_t* PLUGIN_CALL ffmpegVideoDecoder_getFrame(videoDecodeStream_t *stream, long double time) {
	return VideoDecodeStreamObj(stream)->getFrame(time);
}

/************************************
 * Module information
 ************************************/

const videoDecoderInfo_t videoDecoderInfo = {
		80,
		ffmpegVideoDecoder_init,
		ffmpegVideoDecoder_finalize,
		ffmpegVideoDecoder_open,
		ffmpegVideoDecoder_close,
		ffmpegVideoDecoder_setLoop,
		ffmpegVideoDecoder_getLoop,
		ffmpegVideoDecoder_setPosition,
		ffmpegVideoDecoder_getPosition,
		ffmpegVideoDecoder_getFrameWidth,
		ffmpegVideoDecoder_getFrameHeight,
		ffmpegVideoDecoder_getFrameAspect,
		ffmpegVideoDecoder_getFrame
};
