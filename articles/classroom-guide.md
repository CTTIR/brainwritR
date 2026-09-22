# Running and reviewing a classroom brainwriting session

## Plan the activity

brainwritR implements a classroom adaptation of the 6-3-5 method
attributed to Bernd Rohrbach. The original name refers to six people,
three ideas and five minutes. This application instead provides **two
questions per topic** and rotates topics between groups. The 15-2-5
preset means three topics, three rounds and five minutes per round,
intended for approximately 15 participants. It does not enforce a fixed
number of ideas in an answer.

Keep the core of 6-3-5 within the two fields: frame each topic as one
open problem, ask for new ideas in the first field and for developing an
idea already on the sheet in the second. For example: “How might we get
everyone to take an active part in group work? Note up to three new
ideas.” and “Pick up an idea from above (or one of your own) and develop
it further.” Ask participants to read, extend and combine earlier ideas.
Reserve time after writing for discussion; the application organizes
contributions without ranking their quality.

Use pseudonyms. Explain who can read drafts, who will receive exports
and when records will be removed. A modern browser and reachable server
are required. There are no participant accounts. The interface offers
German, English and French through a three-position language slider;
German is the initial default. Select English to follow the interface
labels used in this guide. Language selection changes interface text,
not participant contributions or topic content.

## Start the application

``` r

run_app()
# Participant: http://localhost:3838
# Moderator:   http://localhost:3838/?mod=1
```

The default moderator PIN is `635`, suitable for a local demonstration.
For a classroom deployment, supply a private PIN and the externally
reachable URL:

``` r

run_app(
  db_path = "/srv/brainwriting/data/classroom.sqlite",
  mod_pin = Sys.getenv("MOD_PIN"),
  base_url = "https://brainwriting.example.de",
  port = 3838,
  host = "0.0.0.0",
  poll_ms = 2500
)
```

`DB_PATH`, `MOD_PIN`, `BASE_URL` and `PORT` provide defaults when
corresponding arguments are omitted. Explicit arguments take precedence.
Configuration is resolved at launch, with no database or directory
creation during package loading. The default database is
`data/brainwriting.sqlite`, relative to the working directory. Use an
absolute path for a long-running service.

The QR code of the standard session uses `base_url` exactly. A phone’s
`localhost` refers to that phone; use a URL that devices in the
classroom can reach. The moderator route adds `?mod=1`; do not put this
query string into the participant URL.

## Manage several sessions

One instance can hold and run several sessions at once. The bare
`base_url` always shows the standard session. Further sessions receive a
short code and their own address, for example
`https://brainwriting.example.de/?s=k7m3pq`, with a matching QR code.
After signing in, **All sessions** in the moderator bar opens the
overview at `?mod=1&view=sessions`.

The overview shows each session’s state, mode, participant and
contribution counts, topics and address. **New session** creates an
empty session and opens its setup. **QR code** shows the address as a
large QR code that can be downloaded as PNG. **Restart** creates a new
session with the same settings and opens its setup for review; the
original results stay untouched. **Archive** moves a finished session
out of the active list: it refuses participants but remains readable,
with all exports. **Restore** returns it. **Delete** permanently removes
a session and its database file after confirmation, so export first.

The standard session keeps the bare address and is therefore never
deleted or archived in place. **Reset** clears it after confirmation.
**Archive** stores its finished results as a new archived session and
then clears it for the next activity.

The PIN is entered once per browser tab. The server keeps a login token
in memory and the tab keeps it in session storage, so opening other
sessions does not ask again; **Sign out** ends it and a server restart
signs everyone out. Five wrong PINs pause new logins for 15 seconds,
doubling up to five minutes. Use a long, private PIN, because it now
protects deletion as well.

## Choose a device mode

| Devices | Mode | Authors | Schedule |
|:---|:---|:---|:---|
| Everyone has a phone or laptop | `individual` | People | Parallel timed rounds |
| One device per group table | `group_device` | Groups | Parallel timed rounds |
| One computer for the entire room | `hot_seat` | People | Sequential timed turns within passes |

The mode is selected during setup and cannot change during the session.
All three modes use the same rotation and entry-storage engine.

### Individual devices

The default workflow is unchanged. Prepare 2–6 topics, open the lobby
and share the QR code. People enter a name or pseudonym and join. A
prepared roster adds one-tap name buttons; claimed names are disabled,
with free-text joining still available. A blank free-text name receives
an automatically generated label. Start requires at least as many people
as topics.

The app randomizes balanced groups. With 16 people and three topics, the
sizes are 6, 5 and 5. People use their own devices, read the earlier
contributions on their assigned sheet and answer both questions. Submit
marks both answers as submitted; they remain editable until the round
ends. The moderator sees group progress, can add 60 seconds and can end
a round early.

### Group devices

Each device claims one named group, which becomes the author of its
answers. There is exactly one sheet per topic. Start normally requires
every group to be claimed; the moderator can explicitly start without
all groups.

Reclaiming a group asks for confirmation and reuses the same persistent
author identity. Existing contributions retain their attribution. Close
the old device’s tab after taking over: simultaneous edits from old and
new devices use last-write-wins. Unclaimed groups may join after the
session has started.

### Hot seat

Enter at least one name in the roster, one per line. Saving starts
directly at a handover screen instead of opening a QR lobby. The next
person selects the start button to arm the timer. Finishing saves both
answers and hands over to the next person; skipping a handover creates
no entries. Turn order follows roster arrival order. A pass visits
everyone once, then the topic rotation advances.

Virtual groups use the same balanced assignment as individual mode. If
there are fewer people than topics, an empty starting group receives one
empty sheet so that the shared modulo mapping remains valid. Other sheet
counts remain frozen. The estimated working duration is
`passes * people * turn_secs`; it excludes handovers and pauses. The
default turn is 90 seconds.

Keep a PIN-authenticated moderator tab open on the same computer to add
30 seconds, skip a person, end the rest of a pass, append a latecomer or
finish early while retaining results. Latecomers join the smallest
virtual group and receive turns at the end of the current and remaining
passes. The kiosk derives identity from the active turn and does not
require participant identity in browser storage.

## Try the example question set

Select **Use example questions** in setup to populate three ready-to-use
classroom topics in the currently selected interface language. They
follow the 6-3-5 principle: one open problem per topic, up to three new
ideas in the first field, and building on an idea from the sheet in the
second. The questions remain editable. Clearing the checkbox restores
the previous topic count and questions. The option preserves the
selected device mode, roster and timers. A valid YAML upload replaces
the example selection. Switching language does not translate existing
questions; choose the interface language before enabling the examples.

## Reuse session settings

The setup form and YAML upload prepare the same fields. Upload validates
and prefills the form; it never starts a session. Review the values
before saving. Settings export is available in setup and the lobby. For
hot-seat mode, export before starting if a separate reusable settings
file is needed. A finished RDS snapshot also contains the configuration.

``` yaml
format: brainwriting635-settings/1
mode: individual
rounds: 3
round_secs: 300
turn_secs: 90
topics:
  - title: "Teamwork"
    q1: "What helps a team collaborate?"
    q2: "How can we apply this next week?"
  - title: "Learning spaces"
    q1: "What makes a space useful for learning?"
    q2: "Which change could we try first?"
  - title: "Feedback"
    q1: "What makes feedback actionable?"
    q2: "How can we build it into our routine?"
groups:
  - "North"
  - "South"
  - "West"
participants:
  - "Alex"
  - "Sam"
  - "Robin"
```

The format identifier remains `brainwriting635-settings/1` for
compatibility. Topic count must be 2–6, with nonempty titles and both
questions. Rounds must be an integer from 1–12. Parallel rounds allow
30–1800 seconds; hot-seat turns allow 20–600 seconds. Group names must
be unique and match the topic count. Optional roster names must be
nonempty and unique after trimming.

The upload limit is 100 KiB. YAML expressions are not evaluated.
Validation collects all errors together; unknown keys warn and are
ignored. The moderator PIN is neither loaded from nor written to the
settings file.

## Understand the rotation

Group `g` in round or pass `r` receives topic
`((g - 1 + r - 1) %% K) + 1`:

``` r

k <- 3L
rotation <- outer(seq_len(k), seq_len(k), function(g, r) topic_for(g, r, k))
dimnames(rotation) <- list(paste("Group", 1:k), paste("Round", 1:k))
knitr::kable(rotation)
```

|         | Round 1 | Round 2 | Round 3 |
|:--------|--------:|--------:|--------:|
| Group 1 |       1 |       2 |       3 |
| Group 2 |       2 |       3 |       1 |
| Group 3 |       3 |       1 |       2 |

Every row and column is a permutation of the topics. Across K complete
rounds, every group visits every topic once. Fewer rounds cover a
partial cycle; more rounds repeat topics. A late arrival cannot recover
missed topics automatically.

Each topic begins with one sheet per person in its starting group,
frozen at start, except for the single-sheet group-device mode and empty
hot-seat groups. Within-group index `i` receives sheet
`((i - 1) %% n_sheets) + 1`:

``` r

sheet_for(idx = 1:6, n_sheets = 5)
#> [1] 1 2 3 4 5 1
sheet_for(idx = 1:5, n_sheets = 6)
#> [1] 1 2 3 4 5
```

Six people visiting five sheets share one sheet, retaining both authors’
entries. Five people visiting six sheets leave one untouched in that
round. Unequal group sizes therefore do not guarantee a contribution to
every sheet every round. Completion of either question is voluntary.

## Read the results responsibly

The finished moderator screen separates the contribution list from
descriptive analytics. The four indicators count nonempty saved entries,
average unfiltered word count, submitted share and distinct filtered
terms. Drafts are included. The submitted share uses nonempty stored
entries as its denominator, not the number of theoretically possible
answers or attendance.

Five shared figure functions provide contributions by topic and round,
eight top terms per topic, a co-occurrence network, a wordcloud and
lexical continuity between rounds. The network defaults to at least two
co-occurring entries and at most 40 frequent terms. The wordcloud uses
at most 60 terms. A topic selector filters both. Empty or insufficient
text produces a placeholder.

Text processing splits at Unicode nonletters, lowercases using German
locale, retains words with at least three letters and removes German
Snowball stopwords. Topic-title and question vocabulary are also
excluded by default; the app can disable this filter. There is no
stemming or semantic model. Interface language selection does not change
this German-language text-analysis method; interpret English or French
contributions accordingly.

Term counts retain repeated words, while network edges count the entries
that contain a pair once each. Lexical continuity is the Jaccard overlap
between consecutive rounds’ token sets on each sheet and question,
pooling authors who share the sheet. Missing or empty rounds yield
missing values rather than zero; valid comparisons are averaged by
topic. Word reuse is not evidence of idea quality, understanding or
conceptual development. Read the original answers alongside the figures.

## Choose an export

| Format | Intended use | Contents |
|:---|:---|:---|
| CSV | Statistical or spreadsheet import | Long table, including empty drafts |
| Markdown | Readable discussion protocol | Questions and nonempty answers grouped by sheet |
| RDS | Lossless re-analysis in R | Raw tables, settings, generation time and package version |
| XLSX | Workbook review | Contributions, authors, topics and descriptive summaries |
| PDF | Shareable overview | Exactly two A4 pages using the same figures as the app |

On macOS, the CRAN R build may require
[XQuartz](https://www.xquartz.org/) for Cairo graphics. Install it
before using PDF reports; see the [R Cairo device
documentation](https://search.r-project.org/R/refmans/grDevices/html/cairo.html).

The PDF uses Cairo and patchwork directly, without a runtime dependency
on Pandoc or LaTeX. Its first page contains parameters, indicators and
overview charts. Its second contains networks, a wordcloud and per-topic
summaries. Up to three topics receive separate networks; larger sessions
use one combined network. Export schemas and the PDF retain their German
labels independently of the interface language.

Optional author pseudonymization applies stable arrival-order codes to
all five downloads. It also replaces persistent author IDs and stored
roster settings. On-screen views retain names. This is not complete
anonymization: personal information typed into answers or topic prompts
remains unchanged. Review such content before sharing. CSV values should
be imported as text, and Markdown should be rendered with raw HTML
disabled.

## Persistence, recovery and deployment

SQLite stores the session, topics, authors and entries. Each operation
opens and closes its own connection, with WAL mode and a 5000 ms busy
timeout. Entry uniqueness includes author identity, so two people
sharing a sheet do not replace each other’s contributions. Autosaves
preserve an explicit submission flag.

The main database at `db_path` holds the standard session and the
catalog of further sessions. Each further session is a separate SQLite
file with the same schema in a `sessions/` folder next to it; deleting a
session removes its file. Existing databases migrate in place by adding
`mode`, `current_turn` and `settings_yaml`, and their session becomes
the standard session. Older sessions retain individual mode. The
settings column preserves prepared names and turn duration across
restarts; no sidecar file is needed. Back up before upgrading and avoid
operating newer-mode databases with an older application version.

Every connected client polls the authoritative deadline. Guarded updates
prevent racing clock ticks from advancing twice. Hot-seat updates also
guard the current turn, and stale repeated controls cannot advance a
later person’s turn. Without any connected clients there is no
scheduler: the next connection resumes the expired transition, without
replaying every missed round.

A disconnect triggers a reload after 1.5 seconds. Individual and group
devices resend the identity stored for that session; the server resumes
it or clears it after reset. One device can therefore take part in
several sessions. Keep the same browser and origin. With browser storage
disabled or cleared, automatic identity recovery is unavailable. There
is no offline editing queue.

Typing is debounced for 1200 milliseconds. Main answer fields are
replaced only on genuine session, round or assignment transitions, not
every poll or language change. A delayed save retains its original
editing context. Keystrokes in the last 1–2 seconds of a round or turn
can still lose the race with its boundary. Submit before expiry. The
timer turns red below one minute and can briefly show zero until the
next poll.

Use exactly one R process per data folder, a writable local disk and a
persistent database directory. Do not use replicas, ShinyProxy or
horizontal scaling. Parallel courses can share one instance as separate
sessions. The supplied Docker image runs as UID 10001. Compose expects
an existing Traefik installation, external `proxy` network, TLS resolver
and reachable hostname.

``` bash
mkdir -p docker/data
sudo chown 10001:10001 docker/data
export BW_HOST=brainwriting.example.de
export TRAEFIK_CERTRESOLVER=letsencrypt
export MOD_PIN='replace-with-a-private-pin'
docker compose -f docker/docker-compose.yml up -d --build
```

Mount the whole database directory, including SQLite WAL files and the
`sessions/` folder. For backups, stop the app before copying it or use
SQLite’s online backup API. A live database file copied without its WAL
can omit committed changes. Restarting with the same volume resumes the
session; rebuilding an image does not reset it.

Export before resetting a finished session. Confirmed reset removes
logical session data and clears participant identity on reconnect. It
does not remove old downloads, backups, snapshots or residual SQLite
pages. Apply a retention policy to those copies as well. Self-hosting
and pseudonyms alone do not establish legal compliance.

## Rehearse and troubleshoot

| Symptom | Check |
|:---|:---|
| Phone cannot open the QR link | Public URL, DNS, Wi-Fi reachability and TLS |
| Database cannot be created | Mounted directory ownership for UID 10001 |
| Repeated reconnects | Proxy WebSocket support and service availability |
| Duplicate participant | Cleared storage or a separate browser profile |
| Timer waited while everyone was absent | Expected: no clients means no polling scheduler |
| A sheet lacks a round | Unequal groups, late arrivals or unanswered questions |
| Old work returns after restart | Expected persistence; archive it or start a new session |
| A session link shows “Session nicht gefunden” | The code is mistyped, or the session was deleted |
| Logins pause after wrong PINs | Expected throttling; wait for the shown time |

Tests cover rotation, persistence, configuration, mode transitions,
reconnection, exports, report page counts, session management and
browser workflows. A passing automated suite does not replace a
rehearsal on the institution’s actual devices and network.
