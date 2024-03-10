import numpy as np
import matplotlib.pyplot as plt
from matplotlib.offsetbox import AnchoredText

data_krome   = np.genfromtxt("output.dat", delimiter=",")  # "fort.66",delimiter="  ")
data_semenov = np.genfromtxt("semenov.dat")

# PyKROME output
time  = np.array(data_krome[:, 0])
CO    = np.array(data_krome[:, 26])
HCOj  = np.array(data_krome[:, 418])
H2O   = np.array(data_krome[:, 58])
H2S   = np.array(data_krome[:, 59])

# Semenov+2010 output
time_s = np.array(data_semenov[:, 0])
CO_s   = np.array(data_semenov[:, 6])
HCOj_s = np.array(data_semenov[:, 1])
H2O_s  = np.array(data_semenov[:, 2])
H2S_s  = np.array(data_semenov[:, 3])

# Plotting
fig, axs = plt.subplots(2, 2, figsize=(6, 4), sharex=True, dpi=150)
[[ax1, ax2], [ax3, ax4]] = axs

# CO
ax1.loglog(time_s, CO_s, "dodgerblue", label="Semenov+2010")
ax1.loglog(time, CO, "xk", markersize="5", markevery=2, label="PyKROME")

# HCO+
ax2.loglog(time_s, HCOj_s, "dodgerblue")
ax2.loglog(time, HCOj, "xk", markersize="5", markevery=2)

# H2O
ax3.loglog(time_s, H2O_s, "dodgerblue")
ax3.loglog(time, H2O, "xk", markersize="5", markevery=2)

# H2S
ax4.loglog(time_s, H2S_s, "dodgerblue")
ax4.loglog(time, H2S, "xk", markersize="5", markevery=2)

ax1.set_xlim(1e1, 1e9)
ax1.set_ylim(1e-9, 1e-4)
ax2.set_ylim(1e-13, 1e-8)
ax3.set_ylim(1e-11, 5e-6)
ax4.set_ylim(1e-17, 1e-10)

species = ["CO", "HCO+", "H2O", "H2S"]
for i, ax in enumerate(axs.flatten()):
    at = AnchoredText(species[i], loc="upper left", frameon=False)
    ax.add_artist(at)

ax2.set_yticklabels([])
ax4.set_yticklabels([])
ax3.set_xlabel("time / yr")
ax4.set_xlabel("time / yr")
ax1.set_ylabel("abundances")
ax3.set_ylabel("abundances")
ax1.legend(loc="lower left")
fig.align_labels()
plt.show()
