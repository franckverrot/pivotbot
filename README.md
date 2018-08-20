# pivotbot

pivotbot – A Pivotal Tracker command-line tool


## Requirement

* A Haskell compiler (stack-based setup is supported)
* A Pivotal Tracker API token


## Build

`stack build`


## Usage

```
λ pivotbot – A Pivotal Tracker command-line tool

Usage: pivotbot-exe (--show-story ARG | --list-stories ARG)
  Instant access to your stories

Available options:
  -h,--help                Show this help text
```


## Setup

For the tool to be able to connect to your Pivotal Tracker project, you need to export:

* `TRACKER_API_TOKEN`
* `TRACKER_PROJECT_ID`


## Options

* `--show-story <id>` : Show story's details
* `--list-stories <status>` : Show stories in that status
  * (Supported) Statuses:
    * `unstarted`
    * `started`


## License

BSD3 License.  Franck Verrot. Copyright 2018.
