import numpy as np

from simulation_part.constants import SEED, NUM_MACHINES, PATIENT_DISTRIBUTIONS, SCHEDULE_PATIENTS, MAKE_SCAN
from simulation_part.sim.analysis.data_collector import DataCollector
from simulation_part.sim.sim_objects.machine import Machine
from simulation_part.sim.sim_objects.patient import Patient
from simulation_part.sim.sim_objects.simulation_time import Time


class Simulation:

    def __init__(self, slot_time1, slot_time2, bool_shared_machines=False):
        self.time = Time(0, 0)
        self._bool_shared_machines = bool_shared_machines

        rng = np.random.default_rng(seed=SEED)
        self._coll = DataCollector()

        self._patients = [Patient(i, dists, slot_time, rng, self._coll)
                          for i, (dists, slot_time) in enumerate(zip(PATIENT_DISTRIBUTIONS, [slot_time1, slot_time2]))]
        self._machines = [Machine(self._coll) for _ in range(NUM_MACHINES)]

        self._events = ([{'time':patient.sample_next_call(self.time),
                          'event_type':SCHEDULE_PATIENTS,
                          'patient_type':i} for i, patient in enumerate(self._patients)] +
                        [{'time':Time(1, 0),
                          'event_type':MAKE_SCAN,
                          'machine':i} for i in range(NUM_MACHINES)])

    def next_event(self):
        next_event = self._events.pop(0)
        self.time = next_event['time']
        event_type = next_event['event_type']

        if event_type == SCHEDULE_PATIENTS:
            patient = self._patients[next_event['patient_type']]
            patient_data = patient.generate_patient()
            self._schedule_patient(patient_data)
            self._add_event({'time':patient.sample_next_call(self.time),
                             'event_type':SCHEDULE_PATIENTS,
                             'patient_type':next_event['patient_type']})
        elif event_type == MAKE_SCAN:
            machine = self._machines[next_event['machine']]
            machine.scan_next_patient()
            self._add_event({'time':machine.time_next_patient(),
                             'event_type':MAKE_SCAN,
                             'machine':next_event['machine']})

    def _add_event(self, event):
        self._events.append(event)
        self._events = sorted(self._events, key=lambda x:x['time'])

    def _schedule_patient(self, patient_data):
        if self._bool_shared_machines:
            # Searches for the first available machine
            best_machine, best_time = None, Time(np.inf, 0)
            for machine in self._machines:
                if best_time < machine.time_next_free_slot():
                    best_machine, best_time = machine, machine.time_next_free_slot()
            machine = best_machine
        else:
            machine = self._machines[patient_data['patient_type']]
        machine.schedule_patient(patient_data, self.time)

    def run_analysis(self):
        self._coll.run_analysis()

