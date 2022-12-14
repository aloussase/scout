# scout

CLI tool for searching packages on [Hackage](https://hackage.haskell.org/).

## Install

Build with [cabal](https://cabal.readthedocs.io/en/3.4/getting-started.html) or
grab a binary from the [releases](https://github.com/aloussase/scout/releases)
page.

```bash
git clone https:/github.com/aloussase/scout.git
cd scout
cabal install
```

The project uses GHC version 9.0.2, so make sure you are using that to install.

## Usage

```shell
scout --help
```

```
scout - CLI tool for scouting packages in Hackage

Usage: scout COMMAND [-l|--limit INT] [-f|--format apt, csv] [-s|--select ARG]

  Scout Hackage packages

Available options:
  -l,--limit INT           How many packages to show in the output (default: 16)
  -f,--format apt, csv     Format in which to display search results
                           (default: Apt)
  -s,--select ARG          Fields from package info to output
                           (default: [Description,Downloads,LastUpload,Name,Uri,Votes])
  -h,--help                Show this help text

Available commands:
  search                   Search packages in Hackage
```

## Example

List the first 3 packages:

```shell
scout search aeson --limit 3
```

Find the package with the most downloads and copy its uri to the clipboard:

```shell
scout search megaparsec --limit 1 | awk '/uri/ {print $3}' | xclip -sel clip
```

Output only the name and uri fields (only works in csv mode for the moment):

```shell
scout search scotty --format csv --select name,uri
```

## Neovim - Telescope extension

![scout demo](https://i.imgur.com/cOPE7cz.gif)

The repository contains a
[telescope](https://github.com/nvim-telescope/telescope.nvim) extension in the
`vim` directory that can be used to dynamically search for packages and open
the selected one's Hackage url in the browser.

You can install the extension using
[packer](https://github.com/wbthomason/packer.nvim):

```lua
use { 'aloussase/scout', rtp = 'vim' }
```

And then load the extension:

```lua
require('telescope').load_extension('scout')
```

## License

MIT
