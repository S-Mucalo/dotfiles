#!/usr/bin/env zsh
# Fetch all zsh aliases
alias | awk -F'[ =]' '{print $1}'
