"""
Helper script to create custome calendar for training session with christian(PT)
"""

from datetime import datetime, timedelta
from icalendar import Calendar, Event
import os, pytz


def add_events(start, count, cal, session_start):
    next_session = session_start
    event_no = start
    for rep in range(count):
        event = Event()
        event.add("summary", f"[3] {event_no}/8 Christian Physical Training")
        event.add("dtstart", next_session)
        event.add("dtend", next_session + timedelta(hours=1))
        event.add("dtstamp", datetime.now())
        cal.add_component(event)
        next_session = next_session + timedelta(days=7)
        event_no += 2


cal = Calendar()
wed_session_start = datetime(2022, 12, 7, 7, 0, 0, tzinfo=pytz.utc)  # Wednesdays
sat_session_start = datetime(2022, 12, 10, 8, 0, 0, tzinfo=pytz.utc)  # Saturdays

add_events(start=1, count=4, cal, wed_session_start)
add_events(start=2, count=4, cal, sat_session_start)

directory = os.path.expanduser("~")
f = open(os.path.join(directory, "physical_training.ics"), "wb")
f.write(cal.to_ical())
f.close()
print(f.name)
