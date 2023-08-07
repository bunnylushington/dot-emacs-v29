# Emacs Config v29

My Emacs configuration.   A few notes:

* If you want to use the Slack functionality, you'll need a
  `~/.emacs-slack-config` file (and probably something for
  auth-source-pick-first-password to read).

* The (private) IP4G toolkit requires a configuration directory.

* Although the nano-emacs config is pretty self-contained and can
  presumably be removed, the configuration uses nord-color heavily.

I've used straight.el to do the heavy lifting.  The best way to
bootstrap this is to clone the repo to a clean `~/.emacs.d` and start
up Emacs.  It'll take a while (~ 10 minutes) on the first invocation
but will be in the 5 second range thereafter.  No effort has gone into
optimizing this.
