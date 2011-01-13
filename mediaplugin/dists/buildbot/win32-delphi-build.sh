cd src/mediaplugins/ffmpeg
./autogen.sh
./configure ffmpeg_CFLAGS="-Ilibffmpeg/include" ffmpeg_LIBS="-Llibffmpeg/lib -lavcodec -lavformat -lavutil" libswscale_CFLAGS="-Ilibffmpeg/include" libswscale_LIBS="-Llibffmpeg/lib -lswscale"
make compile
