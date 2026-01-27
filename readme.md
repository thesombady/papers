# Papers -- a CLI reference manager

Papers is a small CLI tool to keep track of your reference and to easily get
your citations in place for LaTeX. It has been developed with a focused on UNIX
the philosophy, where one passes readable output that can be passed between systems.

# Installation
Installation is done via the so `install.sh` script. Beware, the script installs
to `~/.lib/bin` and not `~/.local/bin` due to my preference of developing in this
directory since naming collisions are easier to avoid. Either create this folder
and add it to your path, or modify the install script.

It also relies on two environment variables `$EDITOR` and `$PDF_VIEWER`; these has to
set for the environment, if they are not open `vi` and/or your system determined
application will be used instead.
