#!/usr/bin/env bash

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
WHITE='\033[1;37m'
BOLD='\033[1m'
RESET='\033[0m'

# Detect date command type (macOS vs GNU)
if date -v-1d > /dev/null 2>&1; then
  DATE_TYPE="macos"
else
  DATE_TYPE="gnu"
fi

# Function to normalize author names
normalize_author() {
  awk -F'|' '
  {
    author = $1
    # Normalize author names
    if (author == "rhulenko" || author == "roman-hulenko") author = "Roman Hulenko"
    if (author == "rssh") author = "Ruslan Shevchenko"
    if (author == "otto") author = "Otto Edgar"
    if (author == "fernweh0" || author == "Aleksey Khodakovskiy") author = "Oleksii Khodakivskyi"
    if (length(author) == 0) author = "(unknown)"

    added[author]+=$2
    deleted[author]+=$3
    commits[author]+=$4
  }
  END {
    for (author in added) {
      total = added[author] + deleted[author]
      printf "%d|%s|%d|%d|%d\n", total, author, added[author], deleted[author], commits[author]
    }
  }'
}

# Function to get git stats for a date range (optional parameters)
# Only counts .scala files
get_git_stats() {
  local since=$1
  local until=$2
  local git_cmd="git log --all --numstat --pretty=format:'AUTHOR:%aN'"

  if [ -n "$since" ]; then
    git_cmd="$git_cmd --since=\"$since\""
  fi
  if [ -n "$until" ]; then
    git_cmd="$git_cmd --until=\"$until\""
  fi

  eval "$git_cmd" | awk '
    /^AUTHOR:/ {
      if (author) {
        print author "|" plus "|" minus "|" commits
        plus=0; minus=0; commits=0
      }
      author=substr($0, 8)
      commits++
    }
    /^[0-9]/ && NF==3 && $3 ~ /\.scala$/ {
      plus+=$1
      minus+=$2
    }
    /^$/ {
      if (author) {
        print author "|" plus "|" minus "|" commits
        author=""
        plus=0; minus=0; commits=0
      }
    }
    END {
      if (author) {
        print author "|" plus "|" minus "|" commits
      }
    }
  ' | normalize_author | sort -rn
}

# Function to display author stats table
# Args: stats, threshold (default 0), scale (default 1, use 10 for all-time stats)
display_author_stats() {
  local stats=$1
  local threshold=${2:-0}
  local scale=${3:-1}

  echo "$stats" | awk -F'|' -v green="$GREEN" -v white="$WHITE" -v cyan="$CYAN" -v magenta="$MAGENTA" -v yellow="$YELLOW" -v reset="$RESET" -v bold="$BOLD" -v threshold="$threshold" -v scale="$scale" '
  BEGIN {
    printf "%s%-30s %10s %15s %15s %15s%s\n", bold white, "Author", "Commits", "Lines Added", "Lines Deleted", "Total Changes", reset
    printf "%s%-30s %10s %15s %15s %15s%s\n", cyan, "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━", "━━━━━━━━━━", "━━━━━━━━━━━", "━━━━━━━━━━━━━", "━━━━━━━━━━━━━", reset
  }
  {
    if ($1 > threshold) {
      # Color code based on contribution size (scaled thresholds)
      t1 = 10000 * scale
      t2 = 5000 * scale
      t3 = 1000 * scale
      color = green
      if ($1 > t1) color = bold magenta
      else if ($1 > t2) color = bold yellow
      else if ($1 > t3) color = yellow

      printf "%s%-30s%s %s%10d%s %s%15d%s %s%15d%s %s%15d%s\n",
        white, $2, reset,
        white, $5, reset,
        green, $3, reset,
        cyan, $4, reset,
        color, $1, reset
    }
  }'
}

echo -e "${BOLD}${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"
echo -e "${BOLD}${WHITE}                         Scalus Project Statistics${RESET}"
echo -e "${BOLD}${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"
echo ""

# Count lines of Scala code
echo -e "${BOLD}${YELLOW}📊 Lines of Code${RESET}"
echo -e "${BOLD}${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"

# Count Scala files
total_scala_files=$(find . -name "*.scala" -type f -not -path "*/target/*" -not -path "*/.bloop/*" -not -path "*/.metals/*" | wc -l | awk '{print $1}')
main_scala_files=$(find . -path "*/src/main/*" -name "*.scala" -type f -not -path "*/target/*" -not -path "*/.bloop/*" -not -path "*/.metals/*" | wc -l | awk '{print $1}')
test_scala_files=$(find . -path "*/src/test/*" -name "*.scala" -type f -not -path "*/target/*" -not -path "*/.bloop/*" -not -path "*/.metals/*" | wc -l | awk '{print $1}')

# Count lines of code
total_lines=$(find . -name "*.scala" -type f -not -path "*/target/*" -not -path "*/.bloop/*" -not -path "*/.metals/*" | xargs wc -l 2>/dev/null | tail -1 | awk '{print $1}')
main_lines=$(find . -path "*/src/main/*" -name "*.scala" -type f -not -path "*/target/*" -not -path "*/.bloop/*" -not -path "*/.metals/*" | xargs wc -l 2>/dev/null | tail -1 | awk '{print $1}')
test_lines=$(find . -path "*/src/test/*" -name "*.scala" -type f -not -path "*/target/*" -not -path "*/.bloop/*" -not -path "*/.metals/*" | xargs wc -l 2>/dev/null | tail -1 | awk '{print $1}')

echo -e "${GREEN}Total Scala files:${RESET}  ${BOLD}${WHITE}${total_scala_files}${RESET}"
echo -e "${GREEN}Main files:${RESET}         ${BOLD}${WHITE}${main_scala_files}${RESET}"
echo -e "${GREEN}Test files:${RESET}         ${BOLD}${WHITE}${test_scala_files}${RESET}"
echo ""
echo -e "${GREEN}Total Scala lines:${RESET}  ${BOLD}${WHITE}${total_lines}${RESET}"
echo -e "${GREEN}Main code lines:${RESET}    ${BOLD}${WHITE}${main_lines}${RESET}"
echo -e "${GREEN}Test code lines:${RESET}    ${BOLD}${WHITE}${test_lines}${RESET}"
echo ""

# Git statistics by author (all time)
echo -e "${BOLD}${YELLOW}👥 Contributions by Author${RESET}"
echo -e "${BOLD}${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"

all_time_stats=$(get_git_stats "" "")
if [ -n "$all_time_stats" ]; then
  display_author_stats "$all_time_stats" 0 10
else
  echo -e "${YELLOW}No commits found${RESET}"
fi

echo ""

# Previous month stats
echo -e "${BOLD}${YELLOW}📅 Previous Month Contributions${RESET}"
echo -e "${BOLD}${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"

# Calculate previous month date range
if [ "$DATE_TYPE" = "macos" ]; then
  prev_month_start=$(date -v-1m -v1d -v0H -v0M -v0S "+%Y-%m-%d 00:00:00")
  prev_month_end=$(date -v1d -v-1d -v23H -v59M -v59S "+%Y-%m-%d 23:59:59")
  prev_month_name=$(date -v-1m "+%B %Y")
else
  prev_month_start=$(date -d "$(date +%Y-%m-01) -1 month" "+%Y-%m-%d 00:00:00")
  prev_month_end=$(date -d "$(date +%Y-%m-01) -1 day" "+%Y-%m-%d 23:59:59")
  prev_month_name=$(date -d "$(date +%Y-%m-01) -1 month" "+%B %Y")
fi
echo -e "${CYAN}Period: ${WHITE}$prev_month_name${RESET}"
echo ""

prev_month_stats=$(get_git_stats "$prev_month_start" "$prev_month_end")
if [ -n "$prev_month_stats" ]; then
  display_author_stats "$prev_month_stats" 0
else
  echo -e "${YELLOW}No commits in previous month${RESET}"
fi

echo ""

# Current month stats
echo -e "${BOLD}${YELLOW}📅 Current Month Contributions${RESET}"
echo -e "${BOLD}${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"

if [ "$DATE_TYPE" = "macos" ]; then
  curr_month_start=$(date -v1d -v0H -v0M -v0S "+%Y-%m-%d 00:00:00")
else
  curr_month_start=$(date -d "$(date +%Y-%m-01)" "+%Y-%m-%d 00:00:00")
fi
curr_month_name=$(date "+%B %Y")
echo -e "${CYAN}Period: ${WHITE}$curr_month_name${RESET}"
echo ""

curr_month_stats=$(get_git_stats "$curr_month_start" "now")
if [ -n "$curr_month_stats" ]; then
  display_author_stats "$curr_month_stats" 0
else
  echo -e "${YELLOW}No commits in current month${RESET}"
fi

echo ""

# Monthly breakdown for last year
echo -e "${BOLD}${YELLOW}📊 Monthly Activity (Last 12 Months)${RESET}"
echo -e "${BOLD}${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"
echo ""

printf "${BOLD}${WHITE}%-15s %10s %15s %15s %15s${RESET}\n" "Month" "Commits" "Lines Added" "Lines Deleted" "Total Changes"
printf "${CYAN}%-15s %10s %15s %15s %15s${RESET}\n" "━━━━━━━━━━━━━━━━━━━" "━━━━━━━━━━" "━━━━━━━━━━━" "━━━━━━━━━━━━━" "━━━━━━━━━━━━━"

for i in {11..0}; do
  # Calculate month start and end dates
  if [ "$DATE_TYPE" = "macos" ]; then
    month_start=$(date -v-${i}m -v1d -v0H -v0M -v0S "+%Y-%m-%d 00:00:00")
    month_end=$(date -v-${i}m -v1d -v+1m -v-1d -v23H -v59M -v59S "+%Y-%m-%d 23:59:59")
    month_name=$(date -v-${i}m "+%B %Y")
  else
    month_start=$(date -d "$(date +%Y-%m-01) -${i} month" "+%Y-%m-%d 00:00:00")
    month_end=$(date -d "$(date -d "$(date +%Y-%m-01) -${i} month" +%Y-%m-01) +1 month -1 day" "+%Y-%m-%d 23:59:59")
    month_name=$(date -d "$(date +%Y-%m-01) -${i} month" "+%B %Y")
  fi

  month_stats=$(get_git_stats "$month_start" "$month_end")

  if [ -n "$month_stats" ]; then
    total_commits=$(echo "$month_stats" | awk -F'|' '{sum+=$5} END {print sum+0}')
    total_added=$(echo "$month_stats" | awk -F'|' '{sum+=$3} END {print sum+0}')
    total_deleted=$(echo "$month_stats" | awk -F'|' '{sum+=$4} END {print sum+0}')
    total_changes=$(( ${total_added:-0} + ${total_deleted:-0} ))

    # Color code based on activity level
    if [ "${total_changes:-0}" -gt 10000 ]; then
      color="${BOLD}${MAGENTA}"
    elif [ "${total_changes:-0}" -gt 5000 ]; then
      color="${BOLD}${YELLOW}"
    elif [ "${total_changes:-0}" -gt 1000 ]; then
      color="${YELLOW}"
    else
      color="${GREEN}"
    fi

    printf "${WHITE}%-15s${RESET} ${WHITE}%10d${RESET} ${GREEN}%15d${RESET} ${CYAN}%15d${RESET} ${color}%15d${RESET}\n" \
      "$month_name" "${total_commits:-0}" "${total_added:-0}" "${total_deleted:-0}" "${total_changes:-0}"
  else
    printf "${WHITE}%-15s${RESET} ${WHITE}%10d${RESET} ${GREEN}%15d${RESET} ${CYAN}%15d${RESET} ${GREEN}%15d${RESET}\n" \
      "$month_name" 0 0 0 0
  fi
done

echo ""
echo -e "${BOLD}${CYAN}━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━${RESET}"