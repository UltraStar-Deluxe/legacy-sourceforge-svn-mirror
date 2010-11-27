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
#ifndef _LOGGER_H_
#define _LOGGER_H_

#include "plugin_core.h"
#include <string>

#ifdef __cplusplus

#define logger (Logger::getInstance())
class Logger {
private:
	static Logger _instance;

	Logger() {}
	~Logger() {}

public:
	static const Logger &getInstance() {
		return _instance;
	}

	void log(log_level level, const std::string &msg, const std::string &context) const {
		pluginCore->log(level, msg.c_str(), context.c_str());
	}

	void info(const std::string &msg, const std::string &context) const {
		log(LOG_INFO, msg, context);
	}

	void status(const std::string &msg, const std::string &context) const {
		log(LOG_STATUS, msg, context);
	}

	void warn(const std::string &msg, const std::string &context) const {
		log(LOG_WARN, msg, context);
	}

	void error(const std::string &msg, const std::string &context) const {
		log(LOG_ERROR, msg, context);
	}

	void critical(const std::string &msg, const std::string &context) const {
		log(LOG_CRITICAL, msg, context);
	}
};

#endif /* __cplusplus */

#endif /* _LOGGER_H_ */
