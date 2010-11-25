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
#include "core/plugin_core.h"
#include "ffmpeg_plugin.h"
#include "ffmpeg_audio_decode.h"
#include "ffmpeg_audio_convert.h"
#include "ffmpeg_video_decode.h"

MediaCore_FFmpeg *ffmpegCore;

DECLSPEC_EXPORT const pluginInfo_t* PLUGIN_CALL Plugin_register(const pluginCore_t *core) {
	pluginInitCore(core);
	return &pluginInfo;
}

static BOOL PLUGIN_CALL Plugin_initialize() {
	static bool initialized = false;
	if (initialized)
		return TRUE;
	initialized = TRUE;

	//logger.status("InitializeDecoder", "Plugin_initialize");

	av_register_all();

	ffmpegCore = new MediaCore_FFmpeg();

	// Do not show uninformative error messages by default.
	// FFmpeg prints all error-infos on the console by default what
	// is very confusing as the playback of the files is correct.
	// We consider these errors to be internal to FFMpeg. They can be fixed
	// by the FFmpeg guys only and do not provide any useful information in
	// respect to USDX.
#ifndef ENABLE_FFMPEG_ERROR_OUTPUT
#if LIBAVUTIL_VERSION_MAJOR >= 50
	av_log_set_level(AV_LOG_FATAL);
#else
	// FATAL and ERROR share one log-level, so we have to use QUIET
	av_log_set_level(AV_LOG_QUIET);
#endif //LIBAVUTIL_VERSION_MAJOR
#endif //ENABLE_FFMPEG_ERROR_OUTPUT

	return TRUE;
}

static BOOL PLUGIN_CALL Plugin_finalize() {
	delete ffmpegCore;
	return TRUE;
}

const pluginInfo_t pluginInfo = {
		MAKE_VERSION(0, 0, 0),
		"FFmpeg",
		Plugin_initialize,
		Plugin_finalize,
		&audioDecoderInfo,
		&audioConverterInfo,
		&videoDecoderInfo
};
