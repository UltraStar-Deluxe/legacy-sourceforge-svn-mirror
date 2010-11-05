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
#include "ffmpeg_core.h"
#include "core/logger.h"
#include <sstream>

const uint8_t* STATUS_PACKET = (uint8_t*)"STATUS_PACKET";

static int CDECL ffmpegStreamOpen(URLContext *h, const char *filename, int flags);
static int CDECL ffmpegStreamRead(URLContext *h, uint8_t *buf, int size);
static int CDECL ffmpegStreamWrite(URLContext *h, const unsigned char *buf, int size);
static int64_t CDECL ffmpegStreamSeek(URLContext *h, int64_t pos, int whence);
static int CDECL ffmpegStreamClose(URLContext *h);

#define UNICODE_PROTOCOL_NAME "ufile"
#define UNICODE_PROTOCOL_PREFIX UNICODE_PROTOCOL_NAME ":"

std::string hexVerToStr(unsigned version) {
	unsigned major = (version >> 16) & 0xFF;
	unsigned minor = (version >> 8) & 0xFF;
	unsigned release = version & 0xFF;
	std::stringstream s;
	s << major << "." << minor << "." << release;
	return s.str();
}

void checkVersions() {
	unsigned libVersion;
	unsigned headerVersion;

	#ifdef LIBAVCODEC_VERSION_INT
	libVersion = avcodec_version();
	headerVersion = LIBAVCODEC_VERSION_INT;
	if (libVersion != headerVersion) {
		logger.error("libavcodec header (" + hexVerToStr(headerVersion)
				+ ") and " + "DLL (" + hexVerToStr(libVersion)
				+ ") versions do not match.", "");
	}
	#endif

	#if defined(LIBAVFORMAT_VERSION_INT) && \
	    (LIBAVFORMAT_VERSION_INT >= AV_VERSION_INT(52,20,0))
	libVersion = avformat_version();
	headerVersion = LIBAVFORMAT_VERSION_INT;
	if (libVersion != headerVersion) {
		logger.error("libavformat header (" + hexVerToStr(headerVersion)
				+ ") and " + "DLL (" + hexVerToStr(libVersion)
				+ ") versions do not match.", "");
	}
	#endif

	#if defined(LIBAVUTIL_VERSION_INT) && \
	    (LIBAVUTIL_VERSION_INT >= AV_VERSION_INT(49,8,0))
	libVersion = avutil_version();
	headerVersion = LIBAVUTIL_VERSION_INT;
	if (libVersion != headerVersion) {
		logger.error("libavutil header (" + hexVerToStr(headerVersion) + ") and "
				+ "DLL (" + hexVerToStr(libVersion)
				+ ") versions do not match.", "");
	}
	#endif

	#if defined(LIBSWSCALE_VERSION_INT) && \
	    (LIBSWSCALE_VERSION_INT >= AV_VERSION_INT(0,6,1))
	libVersion = swscale_version();
	headerVersion = LIBSWSCALE_VERSION_INT;
	if (libVersion != headerVersion) {
		logger.error("libswscale header (" + hexVerToStr(headerVersion)
				+ ") and " + "DLL (" + hexVerToStr(libVersion)
				+ ") versions do not match.", "");
	}
	#endif
}

MediaCore_FFmpeg::MediaCore_FFmpeg() {
	checkVersions();

	memset(&utf8FileProtocol, 0, sizeof(URLProtocol));
	utf8FileProtocol.name = UNICODE_PROTOCOL_NAME;
	utf8FileProtocol.url_open = ffmpegStreamOpen;
	utf8FileProtocol.url_read = ffmpegStreamRead;
	utf8FileProtocol.url_write = ffmpegStreamWrite;
	utf8FileProtocol.url_seek = ffmpegStreamSeek;
	utf8FileProtocol.url_close = ffmpegStreamClose;

	av_register_protocol2(&utf8FileProtocol, sizeof(URLProtocol));
}

MediaCore_FFmpeg::~MediaCore_FFmpeg() {
}

std::string MediaCore_FFmpeg::getErrorString(int errorNum) const {
	switch (errorNum) {
	case AVERROR_IO:
		return "AVERROR_IO";
	case AVERROR_NUMEXPECTED:
		return "AVERROR_NUMEXPECTED";
	case AVERROR_INVALIDDATA:
		return "AVERROR_INVALIDDATA";
	case AVERROR_NOMEM:
		return "AVERROR_NOMEM";
	case AVERROR_NOFMT:
		return "AVERROR_NOFMT";
	case AVERROR_NOTSUPP:
		return "AVERROR_NOTSUPP";
	case AVERROR_NOENT:
		return "AVERROR_NOENT";
	case AVERROR_PATCHWELCOME:
		return "AVERROR_PATCHWELCOME";
	default:
		return "AVERROR_#" + errorNum;
	}
}

/*
 @param(formatCtx is a PAVFormatContext returned from av_open_input_file )
 @param(firstVideoStream is an OUT value of type integer, this is the index of the video stream)
 @param(firstAudioStream is an OUT value of type integer, this is the index of the audio stream)
 @returns(@true on success, @false otherwise)
 */
bool MediaCore_FFmpeg::findStreamIDs(AVFormatContext *formatCtx,
		int *firstVideoStream, int *firstAudioStream) const
{
	// find the first video stream
	*firstAudioStream = -1;
	*firstVideoStream = -1;

	for (unsigned i = 0; i < formatCtx->nb_streams; ++i) {
		AVStream *stream = formatCtx->streams[i];

		if ((stream->codec->codec_type == AVMEDIA_TYPE_VIDEO) && (*firstVideoStream < 0)) {
			*firstVideoStream = i;
		}

		if ((stream->codec->codec_type == AVMEDIA_TYPE_AUDIO) && (*firstAudioStream < 0)) {
			*firstAudioStream = i;
		}
	}

	// return true if either an audio- or video-stream was found
	return (*firstAudioStream > -1) || (*firstVideoStream > -1);
}

int MediaCore_FFmpeg::findAudioStreamIndex(AVFormatContext *formatCtx) const {
	// find the first audio stream
	int streamIndex = -1;
	for (unsigned i = 0; i < formatCtx->nb_streams; ++i) {
		AVStream *stream = formatCtx->streams[i];
		if (stream->codec->codec_type == AVMEDIA_TYPE_AUDIO) {
			streamIndex = i;
			break;
		}
	}
	return streamIndex;
}

bool MediaCore_FFmpeg::convertFFmpegToAudioFormat(SampleFormat ffmpegFormat, audioSampleFormat_t *format) const {
	switch (ffmpegFormat) {
	case SAMPLE_FMT_U8:
		*format = asfU8;
		break;
	case SAMPLE_FMT_S16:
		*format = asfS16;
		break;
	case SAMPLE_FMT_S32:
		*format = asfS32;
		break;
	case SAMPLE_FMT_FLT:
		*format = asfFloat;
		break;
	case SAMPLE_FMT_DBL:
		*format = asfDouble;
		break;
	default:
		return false;
	}
	return true;
}

/**
 * UTF-8 Filename wrapper based on:
 * http://www.mail-archive.com/libav-user@mplayerhq.hu/msg02460.html
 */

int CDECL ffmpegStreamOpen(URLContext *h, const char *filename, int flags) {
	// check for protocol prefix ("ufile:") and strip it
	const std::string protPrefix(UNICODE_PROTOCOL_PREFIX);
	std::string utf8Filename(filename);
	if (utf8Filename.compare(0, protPrefix.size(), protPrefix) == 0)
		utf8Filename.erase(0, protPrefix.size());

	int mode;
	switch (flags) {
	case URL_RDWR:
		mode = FILE_OPEN_MODE_READ_WRITE;
		break;
	case URL_WRONLY:
		mode = FILE_OPEN_MODE_WRITE;
		break;
	case URL_RDONLY:
		mode = FILE_OPEN_MODE_READ;
	}

	fileStream_t *stream = pluginCore->fileOpen(utf8Filename.c_str(), mode);
	if (!stream)
		return AVERROR_NOENT;
	h->priv_data = stream;
	return 0;
}

int CDECL ffmpegStreamRead(URLContext *h, uint8_t *buf, int size) {
	fileStream_t *stream = (fileStream_t*)h->priv_data;
	return pluginCore->fileRead(stream, buf, size);
}

int CDECL ffmpegStreamWrite(URLContext *h, const uint8_t *buf, int size) {
	fileStream_t *stream = (fileStream_t*)h->priv_data;
	return pluginCore->fileWrite(stream, buf, size);
}

int64_t CDECL ffmpegStreamSeek(URLContext *h, int64_t pos, int whence) {
	fileStream_t *stream = (fileStream_t*)h->priv_data;
	switch (whence) {
	case AVSEEK_SIZE:
		return pluginCore->fileSize(stream);
	default:
		return pluginCore->fileSeek(stream, pos, whence);
	}
}

int CDECL ffmpegStreamClose(URLContext *h) {
	fileStream_t *stream = (fileStream_t*)h->priv_data;
	pluginCore->fileClose(stream);
	return 0;
}

/* PacketQueue */

PacketQueue::PacketQueue() :
		_firstListEntry(NULL),
		_lastListEntry(NULL),
		_packetCount(0),
		_size(0),
		_abortRequest(false)
{}

PacketQueue::~PacketQueue() {
	flush();
}

void PacketQueue::abort() {
	Mutex::RegionLock lock(_mutex);
	_abortRequest = true;
	_condition.broadcast();
}

bool PacketQueue::isAborted() {
	Mutex::RegionLock lock(_mutex);
	return _abortRequest;
}

int PacketQueue::put(AVPacket *packet) {
	if (!packet)
		return -1;

	if (packet->data != STATUS_PACKET) {
		if (av_dup_packet(packet) < 0)
			return -1;
	}

	AVPacketList *currentListEntry = (AVPacketList*) av_malloc(sizeof(AVPacketList));
	if (!currentListEntry)
		return -1;

	currentListEntry->pkt = *packet;
	currentListEntry->next = NULL;

	{
		Mutex::RegionLock lock(_mutex);

		if (!_lastListEntry)
			_firstListEntry = currentListEntry;
		else
			_lastListEntry->next = currentListEntry;

		_lastListEntry = currentListEntry;
		++_packetCount;

		_size += currentListEntry->pkt.size;
		_condition.signal();
	}

	return 0;
}

/**
 * Adds a status packet (EOF, Flush, etc.) to the end of the queue.
 * StatusInfo can be used to pass additional information to the decoder.
 * Only assign nil or a valid pointer to data allocated with Getmem() to
 * StatusInfo because the pointer will be disposed with Freemem() on a call
 * to Flush(). If the packet is removed from the queue it is the decoder's
 * responsibility to free the StatusInfo data with FreeStatusInfo().
 */
int PacketQueue::putStatus(int statusFlag, void *statusInfo) {
	// create temp. package
	AVPacket *tempPacket = (AVPacket*) av_malloc(sizeof(AVPacket));
	if (!tempPacket)
		return -1;

	// init package
	av_init_packet(tempPacket);
	tempPacket->data = const_cast<uint8_t*> (STATUS_PACKET);
	tempPacket->flags = statusFlag;
	tempPacket->priv = statusInfo;
	// put a copy of the package into the queue
	const int result = put(tempPacket);
	// data has been copied -> delete temp. package
	av_free(tempPacket);
	return result;
}

void PacketQueue::freeStatusInfo(AVPacket *packet) {
	if (packet->priv)
		free(packet->priv);
}

void* PacketQueue::getStatusInfo(AVPacket *packet) {
	return packet->priv;
}

int PacketQueue::get(AVPacket *packet, bool blocking) {
	const int WAIT_TIMEOUT = 10; // timeout in ms

	{
		Mutex::RegionLock lock(_mutex);
		while (true) {
			if (_abortRequest)
				return -1;

			AVPacketList *currentListEntry = _firstListEntry;
			if (currentListEntry) {
				_firstListEntry = currentListEntry->next;
				if (!_firstListEntry)
					_lastListEntry = NULL;
				--_packetCount;

				_size -= currentListEntry->pkt.size;
				*packet = currentListEntry->pkt;
				av_free(currentListEntry);

				return 1;
			} else if (!blocking) {
				return 0;
			} else {
				// block until a new package arrives,
				// but do not wait till infinity to avoid deadlocks
				if (_condition.waitTimeout(_mutex, WAIT_TIMEOUT) == MUTEX_TIMEDOUT) {
					return 0;
				}
			}
		}
	}
}

int PacketQueue::getSize() {
	Mutex::RegionLock lock(_mutex);
	return _size;
}

void PacketQueue::flush() {
	Mutex::RegionLock lock(_mutex);

	AVPacketList *tempListEntry;
	AVPacketList *currentListEntry = _firstListEntry;
	while (currentListEntry) {
		tempListEntry = currentListEntry->next;
		// free status data
		if (currentListEntry->pkt.data == STATUS_PACKET)
			freeStatusInfo(&currentListEntry->pkt);
		// free packet data
		av_free_packet(&currentListEntry->pkt);
		// Note: param must be a pointer to a pointer!
		av_freep(&currentListEntry);
		currentListEntry = tempListEntry;
	}
	_lastListEntry = NULL;
	_firstListEntry = NULL;
	_packetCount = 0;
	_size = 0;
}
