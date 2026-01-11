from simulation_part.constants import HOURS_IN_DAY, PRINT_SIM_PROGRESS
from simulation_part.sim.sim_objects.simulation_time import Time


class Machine:

    def __init__(self, collector, machine_id):
        self._id = machine_id

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
        if PRINT_SIM_PROGRESS:
            print(f"Patient of type {patient_data['patient_type']}: scheduled at {patient_data['scheduled_at']}")
        self._coll.log_patient_scheduled(patient_data)

    def scan_next_patient(self):
        patient_data = self._patient_schedule.pop(0)
        self._coll.log_patient_start_treatment(patient_data)

        end_scan_at = self._start_time_next_patient.copy()
        end_scan_at.hour += patient_data['scan_duration']

        if PRINT_SIM_PROGRESS:
            print(f"Patient of type {patient_data['patient_type']}: treated at   {self._start_time_next_patient}")
            print(f"   - Planned for {patient_data['slot_time']} and it took {patient_data['scan_duration']:.2f}")

        if len(self._patient_schedule) > 0:
            next_patient = self._patient_schedule[0]
            next_scheduled_at = next_patient['scheduled_at']
        else:
            next_scheduled_at = Time(patient_data['scheduled_at'].day+1, 0)
        self._start_time_next_patient = max(next_scheduled_at, end_scan_at)

        self._coll.log_patient_ends_treatment(patient_data, next_scheduled_at, end_scan_at, self._id)
