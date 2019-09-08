# org-clock-stats

Make sense of the data that gets logged when using Emacs [Org journal][org-journal] to clock-in for tasks.

[org-journal]: https://github.com/bastibe/org-journal

## Prerequisites

You will need [Leiningen][1] 2.0 or above installed.

[1]: https://github.com/technomancy/leiningen

## Running

To start a web server for the application, run:

```
PORT=3001 lein run
lein figwheel           # for hot code reloading
```

## License

Copyright © 2019 Ahmad Nazir Raja

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.


## FAQ

### Get all tags

In the journal directory:

```bash
ls -ltra | c 9 | grep -e "[0-9]$" | xargs cat | grep '| \\_  ' | awk -F '|' '{print tolower($2)}' | c 2 : | sed -E s/'[0-9]{2} '//g | sed -E s/' +$'//g | sort | uniq -c | sort -n
```
