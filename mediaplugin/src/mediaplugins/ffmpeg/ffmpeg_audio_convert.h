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
#ifndef _FFMPEG_AUDIO_CONVERT_H_
#define _FFMPEG_AUDIO_CONVERT_H_

#include "ffmpeg_core.h"
#include "core/plugin_audio_convert.h"
#include <math.h>

extern const audioConverterInfo_t audioConverterInfo;

// Note: FFmpeg seems to be using "kaiser windowed sinc" for resampling, so
// the quality should be good.
class FFmpegAudioConvertStream : public AudioConvertStream {
private:
	ReSampleContext *_resampleContext;
	double _ratio;

protected:
	FFmpegAudioConvertStream(const AudioFormatInfo &srcFormatInfo, const AudioFormatInfo &dstFormatInfo) :
		AudioConvertStream(srcFormatInfo, dstFormatInfo),
		_resampleContext(NULL),
		_ratio(0) {}

	bool init() {
		// Note: FFmpeg does not support resampling for more than 2 input channels

		if (_srcFormatInfo.getFormat() != AUDIO_SAMPLE_FORMAT_S16) {
			logger.error("Unsupported format", "TAudioConverter_FFmpeg.Init");
			return false;
		}

		if (_srcFormatInfo.getFormat() != _dstFormatInfo.getFormat()) {
			logger.error("Incompatible formats", "TAudioConverter_FFmpeg.Init");
			return false;
		}

#if LIBAVCODEC_VERSION_INT >= AV_VERSION_INT(52,15,0)
		// use same values as the deprecated audio_resample_init()
		const int TAPS = 16;
		_resampleContext = av_audio_resample_init(
				_dstFormatInfo.getChannels(),
				_srcFormatInfo.getChannels(),
				lround(_dstFormatInfo.getSampleRate()),
				lround(_srcFormatInfo.getSampleRate()),
				SAMPLE_FMT_S16, SAMPLE_FMT_S16,
				TAPS, 10, 0, 0.8);
#else
		_resampleContext = audio_resample_init(
				_dstFormatInfo.getChannels(),
				_srcFormatInfo.getChannels(),
				lround(_dstFormatInfo.getSampleRate()),
				lround(_srcFormatInfo.getSampleRate()));
#endif

		if (!_resampleContext) {
			logger.error("audio_resample_init() failed", "TAudioConverter_FFmpeg.Init");
			return false;
		}

		// calculate ratio
		_ratio = ((double)_dstFormatInfo.getChannels() / _srcFormatInfo.getChannels()) *
		        (_dstFormatInfo.getSampleRate() / _srcFormatInfo.getSampleRate());

		return true;
	}

public:
	virtual ~FFmpegAudioConvertStream() {
		if (_resampleContext)
			audio_resample_close(_resampleContext);
	}

	static FFmpegAudioConvertStream* open(const AudioFormatInfo &srcFormatInfo, const AudioFormatInfo &dstFormatInfo) {
		FFmpegAudioConvertStream *converter = new FFmpegAudioConvertStream(srcFormatInfo, dstFormatInfo);
		if (!converter->init()) {
			delete converter;
			return 0;
		}
		return converter;
	}

	virtual int convert(uint8_t *inputBuffer, uint8_t *outputBuffer, int &inputSize) {
		if (inputSize <= 0) {
			// avoid div-by-zero in audio_resample()
			return (inputSize == 0) ? 0 : -1;
		}

		int inputSampleCount = inputSize / _srcFormatInfo.getFrameSize();
		int outputSampleCount = audio_resample(_resampleContext,
				(short*)outputBuffer, (short*)inputBuffer, inputSampleCount);
		if (outputSampleCount == -1) {
			logger.error("audio_resample() failed", "TAudioConverter_FFmpeg.Convert");
			return -1;
		}
		return outputSampleCount * _dstFormatInfo.getFrameSize();
	}

	/**
	 * Destination/Source size ratio
	 */
	virtual double getRatio() {
		return _ratio;
	}

	virtual int getOutputBufferSize(int inputSize) {
		return ceil(inputSize * getRatio());
	}
};

#endif /* _FFMPEG_AUDIO_CONVERT_H_ */
