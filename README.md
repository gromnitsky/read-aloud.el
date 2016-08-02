# read-aloud.el

A simple interface to TTS engines.

The package uses an external text-to-speech engine (like flite) to
pronounce the word at or near point, the selected region or a whole
buffer.

[gif]

## Installation

1. Setup at least one of the supported TTS engines (see below).

2. Add to `~/.emacs`:

		(add-to-list 'load-path "/the/dir/repo")
		(require 'read-aloud)

## Usage

* `M-x read-aloud-current-word`
* `M-x read-aloud-selection`
* `M-x read-aloud-buf`

To stop reading at any time you either run any of the commands above
_again_, or do `M-x read-aloud-stop`.


## TTS Engines

See `read-aloud-engines` variable in `read-aloud.el` for the current
list.

### flite

... is the easiest one. For example, on Fedora 24:

	# dnf install flite

Test it:

	$ echo hello | flite

Add to `~/.emacs`:

	(setq read-aloud-engine 'flite)

### speech-dispatcher

... contains a daemon that hides from the user all the details of a
chosen tts engine. To communicate w/ the daemon, read-aloud.el employs
`spd-say` CL util.

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

### Microsoft Speech API

TODO


## A Smoke Test

After you have configured your system tts engine, do `M-x
read-aloud-test`. It should open 2 tmp windows: 1 log window & 1 w/ a
sample text, then it should start reading automatically. After it
finishes you may safely kill those 2 buffers.


## Configuration

TODO


## Bugs

* Testes only under Fedora 24 & Emacs 25.1.1-rc1.

* The algo of picking up the portions of the text in `read-aloud-buf`
  is independent of a major mode & suited only for English texts.


## License

MIT.
