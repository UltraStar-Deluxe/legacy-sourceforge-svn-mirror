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
#ifndef _PLUGIN_VIDEO_DECODE_H_
#define _PLUGIN_VIDEO_DECODE_H_

#include "util.h"
#include "plugin_core.h"
#include "logger.h"

#ifdef __cplusplus

class VideoDecodeStream {
public:
	virtual ~VideoDecodeStream() {}

	virtual void setLoop(bool enable) = 0;
	virtual bool getLoop() = 0;

	virtual void setPosition(double time) = 0;
	virtual double getPosition() = 0;

	virtual int getFrameWidth() = 0;
	virtual int getFrameHeight() = 0;
	virtual double getFrameAspect() = 0;
	virtual videoFrameFormat_t getFrameFormat() = 0;	
	
	virtual uint8_t* getFrame(long double time) = 0;
};

#endif /* __cplusplus */

#endif /* _PLUGIN_VIDEO_DECODE_H_ */
