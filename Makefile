ELM ?= elm
ELM_FORMAT ?= elm-format
PYTHON ?= python3

.PHONY: all check test format

all: elm.js

elm.js: src/Main.elm src/Engine.elm elm.json
	$(ELM) make src/Main.elm --optimize --output=elm.js

test-runner.js: src/TestRunner.elm src/Engine.elm elm.json
	$(ELM) make src/TestRunner.elm --output=test-runner.js

format:
	$(ELM_FORMAT) src --yes

check: all test-runner.js
	$(ELM_FORMAT) src --validate

test: check
	$(PYTHON) test_browser.py
