minimacs.d
==========

This is a minimal configuration for using Emacs in old machines (typically servers). It does not contain any external packages. Tested with [GNU Emacs](https://www.gnu.org/software/emacs/) 24.3.

## How to install

```
curl -L https://raw.githubusercontent.com/cbpark/minimacs.d/master/install.sh | $SHELL
```

If you've cloned this using git, please make sure that you have created directories in `.emacs.d` before running Emacs.

```
mkdir -p ~/.emacs.d/{backup,autosave,etc}
```
