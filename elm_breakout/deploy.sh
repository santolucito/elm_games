pushd .;
cd ~/elm_games/elm_breakout/;
elm-make Main.elm;
mv elm.js ~/elm_games/phonegap-start/www/js;
cd ~/elm_games/;
zip -r phonegap-start.zip phonegap-start/;
mv phonegap-start.zip ~/Downloads/;
popd
