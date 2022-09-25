# scout

CLI tool for scouting packages on [Hackage]().

## unix philosophy

Each package output consists of 5 lines, plus a blank line. So to list the first
3 packages:

```shell
scout search aeson | head -n 18
```

## Todo

- [ ] Different output formats
- [ ] No color option

## License

MIT
