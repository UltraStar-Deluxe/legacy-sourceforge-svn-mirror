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
#ifndef _FFMPEG_VIDEO_DECODE_H_
#define _FFMPEG_VIDEO_DECODE_H_

#include "ffmpeg_core.h"
#include "core/plugin_video_decode.h"

// uncomment if you want to see the debug stuff
//#define DEBUG_DISPLAY
//#define DEBUG_FRAMES

extern const videoDecoderInfo_t videoDecoderInfo;

class FFmpegVideoDecodeStream : public VideoDecodeStream {
private:
	bool _opened; //**< stream successfully opened
	bool _eof; //**< end-of-file state

	bool _loop; //**< looping enabled

	AVStream *_stream;
	int _streamIndex;
	AVFormatContext *_formatContext;
	AVCodecContext *_codecContext;
	AVCodec *_codec;

	AVFrame *_avFrame;
	AVFrame *_avFrameRGB;
	
	videoFrameFormat_t _frameFormat;
	enum PixelFormat _pixelFormat;

	uint8_t *_frameBuffer; //**< stores a FFmpeg video frame
	bool _frameTexValid; //**< if true, fFrameTex contains the current frame

#ifdef USE_SWSCALE
	SwsContext *_swScaleContext;
#endif

	double _aspect; //**< width/height ratio

	long double _frameDuration; //**< duration of a video frame in seconds (= 1/fps)
	long double _frameTime; //**< video time position (absolute)
	long double _loopTime; //**< start time of the current loop

	FFmpegVideoDecodeStream();

	bool decodeFrame();
	void synchronizeTime(AVFrame *frame, double &pts);

	bool _open(const Path &filename, videoFrameFormat_t format);
	void close();

public:
	virtual ~FFmpegVideoDecodeStream() {
		close();
	}

	static FFmpegVideoDecodeStream* open(const Path &filename, videoFrameFormat_t format);

	virtual void setLoop(bool enable);
	virtual bool getLoop();

	virtual void setPosition(double time);
	virtual double getPosition();

	virtual int getFrameWidth();
	virtual int getFrameHeight();
	virtual double getFrameAspect();
	virtual videoFrameFormat_t getFrameFormat();
	
	virtual uint8_t* getFrame(long double time);
};

#endif /* _FFMPEG_VIDEO_DECODE_H_ */
