# scout

CLI tool for scouting packages on [Hackage](https://hackage.haskell.org/).

## Usage

```shell
scout --help
```

```
scout - CLI tool for scouting packages in Hackage

Usage: scout COMMAND [-l|--limit INT] [-f|--format apt, csv]

  Scout Hackage packages

Available options:
  -l,--limit INT           How many packages to show in the output (default: 16)
  -f,--format apt, csv     Format in which to display search results
                           (default: Apt)
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

## Todo

- [ ] Different output formats
- [ ] No color option

## License

MIT
