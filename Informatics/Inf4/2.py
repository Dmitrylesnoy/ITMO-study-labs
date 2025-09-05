import xmltojson
import time

timer = time.time()
with open("schedule.json", "w") as f:
    f.write(xmltojson.parse(open("schedule.xml").read()))
    timer_ = time.time()
    print((timer_-timer)*1000)
