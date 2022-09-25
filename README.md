# scout

CLI tool for scouting packages on [Hackage]().

## unix philosophy

Each package output consists of 4 lines, plus a blank line. So to list the first
4 packages:

```shell
scout search aeson | head -n 20
```

## Todo

- [ ] Different output formats
- [ ] No color option

## License

MIT
