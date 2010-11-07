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
#ifndef _FFMPEG_CORE_H_
#define _FFMPEG_CORE_H_

#ifndef __STDC_CONSTANT_MACROS
#define __STDC_CONSTANT_MACROS
#endif
#include <inttypes.h>
#include <string>
extern "C" {
#include <libavcodec/avcodec.h>
#include <libavformat/avformat.h>
#include <libavformat/avio.h>
#include <libavutil/avutil.h>
#include <libavutil/mathematics.h>
#include <libavutil/rational.h> // used for av_rescale_q
#include <libswscale/swscale.h>
}
#include "core/util.h"

class PacketQueue {
private:
	AVPacketList *_firstListEntry;
	AVPacketList *_lastListEntry;
	int _packetCount;
	Mutex _mutex;
	Condition _condition;
	int _size;
	bool _abortRequest;
public:
	PacketQueue();
	~PacketQueue();

	int put(AVPacket *packet);
	int putStatus(int statusFlag, void *statusInfo);
	void freeStatusInfo(AVPacket *packet);
	void* getStatusInfo(AVPacket *packet);
	int get(AVPacket *packet, bool blocking);
	int getSize();
	void flush();
	void abort();
	bool isAborted();
};

extern const uint8_t* STATUS_PACKET;
enum {
	PKT_STATUS_FLAG_EOF = 1, // signal end-of-file
	PKT_STATUS_FLAG_FLUSH, // request the decoder to flush its avcodec decode buffers
	PKT_STATUS_FLAG_ERROR, // signal an error state
	PKT_STATUS_FLAG_EMPTY  // request the decoder to output empty data (silence or black frames)
};

class MediaCore_FFmpeg;

// Note: singleton pattern does not work here as static vars
// are initialized before the plugin itself is initialized.
extern MediaCore_FFmpeg *ffmpegCore;

class MediaCore_FFmpeg {
private:
	URLProtocol utf8FileProtocol;
	Mutex _codecLock;

	std::string hexVerToStr(unsigned version);
	void checkVersions();
	void registerUTF8FileProtocol();
public:
	MediaCore_FFmpeg();
	~MediaCore_FFmpeg();

	std::string getErrorString(int errorNum) const;
	bool findStreamIDs(AVFormatContext *formatCtx, int *firstVideoStream, int *firstAudioStream) const;
	int findAudioStreamIndex(AVFormatContext *formatCtx) const;
	bool convertFFmpegToAudioFormat(SampleFormat ffmpegFormat, audioSampleFormat_t *format) const;

public:
	class AVCodecLock {
	public:
		AVCodecLock() {
			ffmpegCore->_codecLock.lock();
		}

		~AVCodecLock() {
			ffmpegCore->_codecLock.unlock();
		}
	};
};

// FFmpeg compatibility with older versions

#if LIBAVCODEC_VERSION_INT < AV_VERSION_INT(52,64,0)
#define AVMEDIA_TYPE_VIDEO CODEC_TYPE_VIDEO
#define AVMEDIA_TYPE_AUDIO CODEC_TYPE_AUDIO
#endif

#if LIBAVCODEC_VERSION_INT < AV_VERSION_INT(52,26,0)
#define avcodec_decode_video2(avctx, picture, got_picture_ptr, avpkt) \
	avcodec_decode_video((avctx), (picture), (got_picture_ptr), \
			(avpkt)->data, (avpkt)->size)
#endif

#if LIBAVCODEC_VERSION_INT < AV_VERSION_INT(51,30,0)
#define avcodec_decode_audio3(avctx, samples, frame_size_ptr, avpkt) \
	avcodec_decode_audio((avctx), (samples), (frame_size_ptr), \
			(avpkt)->data, (avpkt)->size)
#elif LIBAVCODEC_VERSION_INT < AV_VERSION_INT(52,26,0)
#define avcodec_decode_audio3(avctx, samples, frame_size_ptr, avpkt) \
	avcodec_decode_audio2((avctx), (samples), (frame_size_ptr), \
			(avpkt)->data, (avpkt)->size)
#endif

#if LIBAVFORMAT_VERSION_INT < AV_VERSION_INT(52,69,0)
#define av_register_protocol2(prot, size) \
	av_register_protocol(prot)
#endif

#endif /* _FFMPEG_CORE_H_ */
