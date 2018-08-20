# ASPECT-ui-backend
ASPECT-ui is a user interface to ASPECT (A Spectra Clustering Tool). This repository contains the backend software running on the ASPECT-ui server. It is at this time intended to be a frontend to the data export of [ASPECT](http://www.tls-tautenburg.de/TLS/fileadmin/forschung/meus/ASPECT/ASPECT.html).

====================

ASPECT-ui consist of two parts. (1) A backend part running on the server (this repository) and (2) a web frontend.


Quickstart
========================
You have to fill the database with meaningful data. These are too large for a git repo. Send me an E-Mail. (dev at aspect-ui.de)

1. Install Erlang
2. Install PostgreSQL and create empty Database
3. Clone this Repo
4. Copy aui.config.example to `/etc/aui.config` and adjust values
5. Run `make run`



How to report bugs, send in patches, fix problems
=================================================

Found a bug in ASPECT-ui? Please report it!

At this stage, please use the issue tracker:
https://github.com/bloerg/ASPECT-ui-backend/issues
