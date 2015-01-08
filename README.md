clocky
======

Clock and Weather desktop widget for Linux &amp; Windows.


Microsoft got rid of their Desktop Gadgets.  Well, I liked the weather and clock gadget I was using.  So here, have a weather and clock gadget!

Clocky is built with Lazarus, a cross-platform, Open Source RAD IDE for FreePascal.  It is designed to compile on both Windows and Linux.  And it shouldn't be too much trouble to compile for OSX and other platforms supported by Lazarus.

Clocky pulls its weather data from OpenWeatherMap.org.  To use Clocky, you'll need a free API key from them.  It only takes a couple of seconds to get one.

Clocky supports up to 8 different Profiles.  Each can have its own location and time zone setting.  By default Clocky always launches the first profile.  But you can launch other profiles by providing their name as a command line parameter:

    clocky 4

You can also switch between profiles within the program itself using the menu.


####Installation and Configuration

If you just want to run the Clocky desktop widget, download the /dist directory of this repository to your machine into a directory named 'Clocky'.

Now go to [http://openweathermap.org/appid](http://openweathermap.org/appid "Open Weather Map's API page") and get an API Key.  It'll just take a moment.

To run the program, in Windows, simply run:

    Clocky.exe

In Linux, run


    ./Clocky


####Todo's
- Geoip check on start to configure first profile.
- Add Yahoo, Weather.com, and other profiles.


####Acknowledgments
 - Clocky's icons are from the VCloud collection: [http://vclouds.deviantart.com/gallery/#/d2ynulp](http://vclouds.deviantart.com/gallery/#/d2ynulp)