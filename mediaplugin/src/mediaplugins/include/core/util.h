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
#ifndef _UTIL_H_
#define _UTIL_H_

#include <string>
#include "plugin_core.h"

class AudioFormatInfo {
private:
    double _sampleRate;
    uint8_t _channels;
    audioSampleFormat_t _format;
    int _frameSize;

    void updateFrameSize() {
    	_frameSize = g_audioSampleSize[_format] * _channels;
    }

public:
	double getSampleRate() const { return _sampleRate; }
	void setSampleRate(double sampleRate) { _sampleRate = sampleRate; }

	uint8_t getChannels() const { return _channels; }
	void setChannels(uint8_t channels) {
		_channels = channels;
		updateFrameSize();
	}

	audioSampleFormat_t getFormat() const { return _format; }
    void setFormat(audioSampleFormat_t sampleFormat) {
    	_format = sampleFormat;
    	updateFrameSize();
    }

	long getFrameSize() const { return _frameSize; }
	double getBytesPerSec() const { return _frameSize * _sampleRate; }

public:
	AudioFormatInfo() :
		_sampleRate(0),
		_channels(0),
		_format(AUDIO_SAMPLE_FORMAT_UNKNOWN),
		_frameSize(0)
	{}

	AudioFormatInfo(int channels, int sampleRate, audioSampleFormat_t sampleFormat) :
		_sampleRate(sampleRate), _channels(channels), _format(sampleFormat)
	{
		updateFrameSize();
	}

	AudioFormatInfo(audioFormatInfo_t *info) :
		_sampleRate(info->sampleRate),
		_channels(info->channels),
		_format(info->format)
	{
		updateFrameSize();
	}

    /**
     * Returns the inverse ratio of the size of data in this format to its
     * size in a given target format.
     * Example: SrcSize*SrcInfo.GetRatio(TgtInfo) = TgtSize
     */
    double getRatio(const AudioFormatInfo &targetInfo) const {
    	return (targetInfo.getFrameSize() / this->_frameSize) *
    	       (targetInfo.getSampleRate() / this->_sampleRate);
    }

    void toCStruct(audioFormatInfo_t *info) const {
    	info->channels = _channels;
    	info->format = _format;
    	info->sampleRate = _sampleRate;
    }

    //AudioFormatInfo copy();
};

class IPath {
private:
	std::string _filename;
public:
	IPath(const char *filename) :
		_filename(filename)
	{
		// TODO
	}

	IPath(std::string filename) :
		_filename(filename)
	{
		// TODO
	}

	std::string toNative() const {
		// TODO
		return _filename;
	}

	std::string toUTF8() const {
		// TODO
		return _filename;
	}

	bool isFile() const {
		// TODO
		return true;
	}
};

extern "C" {
PLUGIN_CALL int threadMainRoutine(void *data);
}

class Thread {
private:
	thread_t *_thread;
public:
	Thread() : _thread(0) {}
	virtual ~Thread() {}

	virtual int run() = 0;

	void start() {
		_thread = pluginCore->threadCreate(threadMainRoutine, this);
	}

	/**
	 * Get the 32-bit thread identifier for the current thread.
	 */
	static uint32_t getCurrentThreadID() {
		return pluginCore->threadCurrentID();
	}

	/**
	 * Get the 32-bit thread identifier for the specified thread,
	 * equivalent to SDL_ThreadID() if the specified thread is NULL.
	 */
	uint32_t getThreadID() {
		return pluginCore->threadGetID(_thread);
	}

	/**
	 * Wait a specified number of milliseconds before returning.
	 */
	static void sleep(uint32_t ms) {
		pluginCore->threadSleep(ms);
	}

	/**
	 * Wait for a thread to finish.
	 * The return code for the thread function is placed in the area
	 * pointed to by 'status', if 'status' is not NULL.
	 */
	void wait(int *status) {
		if (_thread) {
			pluginCore->threadWait(_thread, status);
		}
	}

	void wait() {
		int status;
		if (_thread) {
			pluginCore->threadWait(_thread, &status);
		}
	}
};

class Condition;

class Mutex {
private:
	friend class Condition;
	mutex_t *_mutex;
public:
	Mutex() {
		_mutex = pluginCore->mutexCreate();
	}

	~Mutex() {
		pluginCore->mutexDestroy(_mutex);
	}

	/**
	 * Lock the mutex
	 * Returns 0, or -1 on error
	 */
	int lock() {
		return pluginCore->mutexLock(_mutex);
	}

	/**
	 * Unlock the mutex
	 * It is an error to unlock a mutex that has not been locked by
	 * the current thread, and doing so results in undefined behavior.
	 *
	 * Returns 0, or -1 on error
	 */
	int unlock() {
		return pluginCore->mutexUnlock(_mutex);
	}

	class RegionLock {
	private:
		Mutex *_mutex;
	public:
		RegionLock(Mutex &mutex) :
			_mutex(&mutex)
		{
			_mutex->lock();
		}

		~RegionLock() {
			_mutex->unlock();
		}
	};
};

class Condition {
private:
	cond_t *_cond;
public:
	Condition() {
		_cond = pluginCore->condCreate();
	}

	~Condition() {
		pluginCore->condDestroy(_cond);
	}

	/**
	 * Wait on the condition variable, unlocking the provided mutex.
	 * The mutex must be locked before entering this function!
	 * The mutex is re-locked once the condition variable is signaled.
	 * Returns 0 when it is signaled, or -1 on error.
	 */
	int wait(const Mutex &mutex) {
		return pluginCore->condWait(_cond, mutex._mutex);
	}

	/*
	 * Waits for at most 'ms' milliseconds, and returns 0 if the condition
	 * variable is signaled, SDL_MUTEX_TIMEDOUT if the condition is not
	 * signaled in the allotted time, and -1 on error.
	 * On some platforms this function is implemented by looping with a delay
	 * of 1 ms, and so should be avoided if possible.
	*/
	int waitTimeout(const Mutex &mutex, uint32_t ms) {
		return pluginCore->condWaitTimeout(_cond, mutex._mutex, ms);
	}

	/**
	 * Restart one of the threads that are waiting on the condition variable.
	 * Returns 0 or -1 on error.
	 */
	int signal() {
		return pluginCore->condSignal(_cond);
	}

	/**
	 * Restart all threads that are waiting on the condition variable.
	 * Returns 0 or -1 on error.
	 */
	int broadcast() {
		return pluginCore->condBroadcast(_cond);
	}
};

#endif /* _UTIL_H_ */
