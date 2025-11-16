from simulation_part.constants import HOURS_IN_DAY
from simulation_part.sim.sim_objects.simulation_time import Time


class Machine:

    def __init__(self, collector):
        self._start_time_next_patient = Time(0, 0)
        self._next_free_slot = Time(0, 0)
        self._patient_schedule = []

        self._coll = collector

    def time_next_patient(self):
        return self._start_time_next_patient.copy()

    def time_next_free_slot(self):
        return self._next_free_slot.copy()

    def schedule_patient(self, patient_data, curr_time):
        if self._next_free_slot.day <= curr_time.day:
            # Ensures that patient can only be planned for the next day
            self._next_free_slot = Time(curr_time.day+1, 0)

        schedule_time = self._next_free_slot.copy()
        if schedule_time.hour + patient_data['slot_time'] > HOURS_IN_DAY:
            # The patient is scheduled at the start of the next day
            self._next_free_slot = Time(self._next_free_slot.day+1, patient_data['slot_time'])
            schedule_time = Time(schedule_time.day+1, 0)
        else:
            self._next_free_slot.hour += patient_data['slot_time']

        patient_data['scheduled_at'] = schedule_time
        self._patient_schedule.append(patient_data)
        print(f"Patient of type {patient_data['patient_type']}: scheduled at {patient_data['scheduled_at']}")

    def scan_next_patient(self):
        patient_data = self._patient_schedule.pop(0)

        end_scan_at = self._start_time_next_patient.copy()
        end_scan_at.hour += patient_data['scan_duration']

        print(f"Patient of type {patient_data['patient_type']}: treated at   {self._start_time_next_patient}")
        print(f"   - Planned for {patient_data['slot_time']} and it took {patient_data['scan_duration']:.2f}")

        next_scheduled_at = self._patient_schedule[0]['scheduled_at']
        self._start_time_next_patient = max(next_scheduled_at, end_scan_at)
