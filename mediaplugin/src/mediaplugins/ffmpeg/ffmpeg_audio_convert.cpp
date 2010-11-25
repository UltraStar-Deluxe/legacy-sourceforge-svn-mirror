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
#include "ffmpeg_audio_convert.h"

/************************************
 * C Interface
 ************************************/

#define ConvertStreamObj(ptr) reinterpret_cast<FFmpegAudioConvertStream*>(ptr)

static BOOL PLUGIN_CALL ffmpegAudioConverter_init() {
	return TRUE;
}

static BOOL PLUGIN_CALL ffmpegAudioConverter_finalize() {
	return TRUE;
}

static audioConvertStream_t* PLUGIN_CALL ffmpegAudioConverter_open(audioFormatInfo_t *inputFormat, audioFormatInfo_t *outputFormat) {
	return (audioConvertStream_t*)FFmpegAudioConvertStream::open(inputFormat, outputFormat);
}

static void PLUGIN_CALL ffmpegAudioConverter_close(audioConvertStream_t *stream) {
	delete ConvertStreamObj(stream);
}

static int PLUGIN_CALL ffmpegAudioConverter_convert(audioConvertStream_t *stream, uint8_t *input, uint8_t *output, int *numSamples) {
	return ConvertStreamObj(stream)->convert(input, output, *numSamples);
}

static int PLUGIN_CALL ffmpegAudioConverter_getOutputBufferSize(audioConvertStream_t *stream, int inputSize) {
	return ConvertStreamObj(stream)->getOutputBufferSize(inputSize);
}

static double PLUGIN_CALL ffmpegAudioConverter_getRatio(audioConvertStream_t *stream) {
	return ConvertStreamObj(stream)->getRatio();
}

/************************************
 * Module information
 ************************************/

const audioConverterInfo_t audioConverterInfo = {
		70,
		ffmpegAudioConverter_init,
		ffmpegAudioConverter_finalize,
		ffmpegAudioConverter_open,
		ffmpegAudioConverter_close,
		ffmpegAudioConverter_convert,
		ffmpegAudioConverter_getOutputBufferSize,
		ffmpegAudioConverter_getRatio
};
