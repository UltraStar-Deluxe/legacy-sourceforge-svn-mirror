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
#ifndef _PLUGIN_AUDIO_CONVERT_H_
#define _PLUGIN_AUDIO_CONVERT_H_

#include "util.h"
#include "plugin_core.h"
#include "logger.h"

#ifdef __cplusplus

class AudioConvertStream {
protected:
	AudioFormatInfo _srcFormatInfo;
	AudioFormatInfo _dstFormatInfo;

	AudioConvertStream(const AudioFormatInfo &srcFormatInfo, const AudioFormatInfo &dstFormatInfo) {
		_srcFormatInfo = srcFormatInfo;
		_dstFormatInfo = dstFormatInfo;
	}

public:
	virtual ~AudioConvertStream() {};

	/**
	 * Converts the InputBuffer and stores the result in OutputBuffer.
	 * If the result is not -1, inputSize will be set to the actual number of
	 * input-buffer bytes used.
	 * Returns the number of bytes written to the output-buffer or -1 if an error occured.
	 */
	virtual int convert(uint8_t *inputBuffer, uint8_t *outputBuffer, int &inputSize) = 0;

	/**
	 * Destination/Source size ratio
	 */
	virtual double getRatio() = 0;

	/**
	 * Size of an output buffer needed to store the result of a converted buffer with
	 * an input size of inputSize bytes.
	 */
	virtual int getOutputBufferSize(int inputSize) = 0;
};

#endif /* __cplusplus */

#endif /* _PLUGIN_AUDIO_CONVERT_H_ */
