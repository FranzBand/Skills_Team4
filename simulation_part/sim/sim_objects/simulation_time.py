class Time:
    def __init__(self, day, hour):
        self.day = day
        self.hour = hour

    def __lt__(self, other):
        if self.day == other.day:
            return self.hour < other.hour
        else:
            return self.day < other.day

    def __str__(self):
        return f"{self.day}D:{self.hour:.2f}H"

    def copy(self):
        return Time(self.day, self.hour)
