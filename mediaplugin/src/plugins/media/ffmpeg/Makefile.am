lib_LTLIBRARIES = libffmpegplugin.la
libffmpegplugin_la_SOURCES = ../core/plugin_core.cpp ../core/util.cpp ../core/logger.cpp ffmpeg_core.cpp ffmpeg_audio_convert.cpp ffmpeg_audio_decode.cpp ffmpeg_video_decode.cpp ffmpeg_plugin.cpp
libffmpegplugin_la_CXXFLAGS = -I../include -Wall \
	$(ffmpeg_CFLAGS) \
	$(libswscale_CFLAGS)
libffmpegplugin_la_LDFLAGS = -shared -dynamic \
	$(ffmpeg_LIBS) \
	$(libswscale_LIBS) \
	$(LDFLAGS)

compile: libffmpegplugin.la
	cp .libs/*$(LIB_EXT) $(PLUGIN_DIR)
