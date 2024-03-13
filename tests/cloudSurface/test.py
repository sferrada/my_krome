"""
    The following script provides a benchmark for
    gas-grain chemistry with KROME using the PyKROME
    Python interface. It is based on the original
    test.f90 file provided with the KROME source code.

    Initial conditions and species abundances have been
    adopted from Semenov et al. 2010.
"""
import numpy as np
import ctypes as ctypes
from pykrome import PyKROME

if __name__ == "__main__":
    pyk = PyKROME()
    pyk.lib.krome_init()

    # Define KROME params
    x_H = 2e4
    spy = pyk.krome_seconds_per_year
    Tgas = ctypes.c_double(1e1)
    adust = ctypes.c_double(1e-5)
    adust2 = ctypes.c_double(1e-5**2)
    adust3 = ctypes.c_double(1e-5**3)
    CR_ion = ctypes.c_double(1.3e-17)
    Av_opacity = ctypes.c_double(1e1)

    # Call user commons for opacity, CR rate, Tdust, etc.
    pyk.lib.krome_set_user_av(Av_opacity)
    pyk.lib.krome_set_user_tdust(Tgas)
    pyk.lib.krome_set_user_gsize(adust)
    pyk.lib.krome_set_user_gsize2(adust2)
    pyk.lib.krome_set_user_crflux(CR_ion)

    # Set initial abundances
    x_min = 1e-40
    abund = np.ones(pyk.krome_nmols) * x_min
    abund[pyk.krome_idx_H2]  = 0.50e+00 * x_H
    abund[pyk.krome_idx_HE]  = 9.00e-02 * x_H
    abund[pyk.krome_idx_Cj]  = 1.20e-04 * x_H
    abund[pyk.krome_idx_N]   = 7.60e-05 * x_H
    abund[pyk.krome_idx_O]   = 2.56e-04 * x_H
    abund[pyk.krome_idx_Sj]  = 8.00e-08 * x_H
    abund[pyk.krome_idx_SIj] = 8.00e-09 * x_H
    abund[pyk.krome_idx_FEj] = 3.00e-09 * x_H
    abund[pyk.krome_idx_NAj] = 2.00e-09 * x_H
    abund[pyk.krome_idx_MGj] = 7.00e-09 * x_H
    abund[pyk.krome_idx_CLj] = 1.00e-09 * x_H
    abund[pyk.krome_idx_Pj]  = 2.00e-10 * x_H

    # Compute electrons abundance (neutral cloud)
    abund[pyk.krome_idx_E] = pyk.lib.krome_get_electrons(abund[:])

    # Get and set dust initial abundance
    mmw = 1.43  # mean molecular weight
    dust_rho0 = 3.0  # material density, g cm-3
    dust_to_gas = 1e-2  # dust-to-gas mass ratio

    rho_gas = pyk.lib.krome_get_rho(abund[:])
    dust_mass = 4. / 3. * np.pi * adust3.value
    dust_abund = dust_to_gas * rho_gas / dust_mass / mmw

    abund[pyk.krome_idx_GRAIN0] = dust_abund
    pyk.lib.krome_set_user_xdust(dust_abund)

    # Set initial time and time step
    d_time = ctypes.c_double(1.0 * spy)
    t_curr = ctypes.c_double(0.0)
    output = []
    n_step = 0

    # Run KROME
    while t_curr.value <= 1e9 * spy:
        if np.mod(n_step, 10) == 0:
            print(f"n_step = {n_step:3d}; t_curr = {t_curr.value / spy:.2e} yr")

        # Call KROME
        pyk.lib.krome(abund[:], ctypes.byref(Tgas), ctypes.byref(d_time))

        # Update time
        t_curr = ctypes.c_double(t_curr.value + d_time.value)
        d_time = ctypes.c_double(np.maximum(1e2, t_curr.value / 3e0))
        n_step += 1

        # Append output
        out_curr = np.insert(
            np.where(
                abund < x_min,
                x_min,
                abund / x_H
            ),
            0,
            t_curr.value / spy
        )
        output.append(out_curr)

    # Write output
    output = np.array(output)
    np.savetxt("output.dat", output, fmt="%+15.8e", delimiter=",")

    print(f"Finished. Number of steps = {n_step}")
    print(f"That's all! have a nice day :)")

