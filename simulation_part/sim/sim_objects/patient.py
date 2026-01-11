import numpy as np

from simulation_part.constants import HOURS_IN_DAY
from simulation_part.sim.sim_objects.simulation_time import Time


class Patient:

    def __init__(self, i, dists, slot_time, rng, collector):
        self._patient_type = i
        self._arrival_dist, self._scan_dist = dists
        self._slot_time = slot_time

        self._rng = rng
        self._coll = collector

    def sample_next_call(self, time):
        time_until_call = _sample_random_variable(self._rng, self._arrival_dist)
        day, hour = time.day, time.hour + time_until_call
        if hour > HOURS_IN_DAY:
            # Continues with the remaining wait time on the next day
            day += 1
            hour -= HOURS_IN_DAY
        return Time(day, hour)

    def generate_patient(self):
        return {'patient_type': self._patient_type,
                'slot_time': self._slot_time,
                'scan_duration': _sample_random_variable(self._rng, self._scan_dist)}


def _sample_random_variable(rng, dist):
    if dist[0] == 'normal':
        (mu, sig) = dist[1]
        return rng.normal(loc=mu, scale=sig)
    elif dist[0] == 'lognormal':
        (mu, sig) = dist[1]
        # The mean and standard deviation of the underlying normal distribution
        log_sig = np.sqrt(np.log(1 + (sig / mu) ** 2))
        log_mu = np.log(mu) - 0.5 * log_sig ** 2
        return rng.lognormal(mean=log_mu, sigma=log_sig)
    elif dist[0] == 'exponential':
        (lambda_inv,) = dist[1]
        return rng.exponential(scale=lambda_inv)
