# ffmpeg -i 'trunks_vid/trunks11_out/trunk_47e8301c-%d.png' -c:v libx264 -r 25 -preset veryslow -crf 22 \
#     -s:v 1080x1080 -aspect 1:1 -bufsize 8000K -maxrate 60000K -f mp4 test.mp4
ffmpeg -i trunk11_out/trunk_47e8301c-%d.png -pix_fmt yuv420p -crf 25 -r 25 -s 1024x1024 to_copy/trunk11lp.mp4
ffmpeg -i trunk13_out/trunk_4b700021-%d.png -pix_fmt yuv420p -crf 25 -r 25 -s 1024x1024 to_copy/trunk13lp.mp4
ffmpeg -i trunk15_out/trunk_49373ce1-%d.png -pix_fmt yuv420p -crf 25 -r 25 -s 1024x1024 to_copy/trunk15lp.mp4
ffmpeg -i trunk18_out/trunk_55d91895-%d.png -pix_fmt yuv420p -crf 25 -r 25 -s 1024x1024 to_copy/trunk18lp.mp4

