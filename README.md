# read-aloud.el

A simple interface to TTS engines.

The package uses an external text-to-speech engine (like flite) to
pronounce the word at or near point, the selected region or a whole
buffer.

![A screenshot of running read-aloud.el](https://raw.github.com/gromnitsky/read-aloud.el/master/test/alice.gif)

## Requirements

* Emacs 24.4+
* A working TTS engine that has a CLI to it.

## Installation

1. Setup at least one of the supported TTS engines (see below).

2. Add to `~/.emacs`:

		(load-file "/the/repo/dir/read-aloud.el")

## Usage

* `M-x read-aloud-this`
* `M-x read-aloud-buf`

To stop reading at any time you either run any of the commands above
_again_, or do `M-x read-aloud-stop`.


## Supported TTS Engines

### speech-dispatcher

... is the default one in read-aloud.el. It contains a daemon that
hides from the user all the details of a chosen tts engine. To
communicate w/ the daemon, read-aloud.el employs `spd-say` CL util.

On Fedora 24:

	# dnf install speech-dispatcher-flite speech-dispatcher-utils

	$ mkdir ~/.config/speech-dispatcher
	$ cp /etc/speech-dispatcher/speechd.conf !$
	$ $EDITOR !$

For example:

	$ grep '^[^#]' ~/.config/speech-dispatcher/speechd.conf
	LogLevel  3
	LogDir  "default"
	DefaultRate  40
	DefaultVolume 100
	AudioOutputMethod "alsa"
	DefaultModule flite

Test it:

	$ spd-say hello

### flite

... is the easiest one to use. For example, on Fedora 24:

	# dnf install flite

Test it:

	$ echo hello | flite

Add to `~/.emacs`:

	(setq read-aloud-engine 'flite)

### Microsoft Speech API

[Jampal](http://jampal.sourceforge.net/ptts.html) provides a CL
interface to SAPI. Install it, then test via:

	> echo hello | cscript "C:\Program Files\Jampal\ptts.vbs"


## Configuration

To add/modify a tts engine, you'll need to edit `read-aloud-engines`
plist. Here is the example for Windows:

	(plist-put read-aloud-engines 'jampal
	  '(cmd "cscript"
			args ("C:\\Program Files\\Jampal\\ptts.vbs" "-r" "8")) )

`args` should be a list or nil. To select a new entry,

	(setq read-aloud-engine 'jampal)

The CL util that communicates w/ the engine must wait until the text
was pronounced (e.g. not exit immediately), otherwise
`(read-aloud-buf)` won't be able to distinguish whether it's time to
feed the engine w/ another chunk of the text. This is why we use
spd-say w/ `-w` CLO.

You can edit the face that `(read-aloud-buf)` uses w/ the usual

	M-x customize-face RET read-aloud-text-face


## A Smoke Test

After you have configured your system tts engine, do

	M-x eval-expression RET (read-aloud-test) RET

It should open 2 tmp windows: 1 log window & 1 w/ a sample text, then
it should start reading automatically. After it finishes you may
safely kill those 2 buffers.


## Bugs

* Testes only under Fedora 24 & Emacs 25.1.1-rc1.

* The algo of picking up the portions of the text in `read-aloud-buf`
  is independent of a major mode & suited only for English texts.


## License

MIT.
