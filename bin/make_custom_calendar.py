#!/usr/local/bin/python3
"""
Helper script to create custome calendar for training session with christian(PT)

python -m venv venv
source venv/bin/activate
pip install icalendar

Use separte google calendar for easy deletions !!
"""

from datetime import datetime, timedelta
from icalendar import Calendar, Event
import os, pytz

# Use below in the code - and get intput from user!!
pt_session_count = "9"
start_date1="14/7/2023 0730"
start_date2="18/7/2023 0700"
date1_event_index=1
date2_event_index=2


def add_events(start, count, cal, session_start):
    next_session = session_start
    event_no = start
    for rep in range(count):
        event = Event()
        event.add("summary", f"{event_no}/8 of package {pt_session_count} Christian Physical Training")
        event.add("dtstart", next_session)
        event.add("dtend", next_session + timedelta(hours=1))
        event.add("dtstamp", datetime.now())
        cal.add_component(event)
        next_session = next_session + timedelta(days=7)
        event_no += 2


cal = Calendar()
wed_session_start = datetime(2023, 7, 14, 7, 30, 0, tzinfo=pytz.timezone('Europe/London'))  # Tuesday
sat_session_start = datetime(2023, 7, 18, 7,  0, 0, tzinfo=pytz.timezone('Europe/London'))  # Friday

add_events(start=date1_event_index, count=4, cal=cal, session_start=wed_session_start)
add_events(start=date2_event_index, count=4, cal=cal, session_start=sat_session_start)

directory = os.path.expanduser("~/Downloads")
f = open(os.path.join(directory, "physical_training.ics"), "wb")
f.write(cal.to_ical())
f.close()
print(f.name)
