# Executable Environment for OSF Project [g8kbu](https://osf.io/g8kbu/)

This repository was automatically generated as part of a project to test the reproducibility of open science projects hosted on the Open Science Framework (OSF).

**Project Title:** Social Smartphone Apps Do Not Capture Attention Despite Their Perceived High Reward Value

**Project Description:**
> Smartphones have been shown to distract people from their main tasks (e.g., studying, working), but the psychological mechanisms underlying these distractions are not clear yet. In a  preregistered experiment, we tested whether the distracting nature of smartphones stems from their high associated (social) reward value. Participants (N = 117) performed a visual search task while they were distracted by (a) high social reward apps (e.g., Facebook app icon + notification sign), (b) low social reward apps (e.g., Facebook app icon), and (c) no social reward apps (e.g., Weather app icon). We expected that high social reward app icons would slow down search, especially when people were deprived of their smartphones. Surprisingly, high social reward (vs. low or no social reward) apps did not impair visual search performance, yet in a survey (N = 158) participants indicated to perceive these icons as more rewarding. Our results demonstrate that even if people perceive social smartphone apps as more rewarding than nonsocial apps, this may not manifest in behavior.

**Original OSF Page:** [https://osf.io/g8kbu/](https://osf.io/g8kbu/)

---

**Important Note:** The contents of the `g8kbu_src` folder were cloned from the OSF project on **12-03-2025**. Any changes made to the original OSF project after this date will not be reflected in this repository.

The `DESCRIPTION` file was automatically added to make this project Binder-ready. For more information on how R-based OSF projects are containerized, please refer to the `osf-to-binder` GitHub repository: [https://github.com/Code-Inspect/osf-to-binder](https://github.com/Code-Inspect/osf-to-binder)

## flowR Integration

This version of the repository has the **[flowR Addin](https://github.com/flowr-analysis/rstudio-addin-flowr)** preinstalled. flowR allows visual design and execution of data analysis workflows within RStudio, supporting better reproducibility and modular analysis pipelines.

To use flowR, open the project in RStudio and go to `Addins` > `flowR`.

## How to Launch:

**Launch in your Browser:**

🚀 **MyBinder:** [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/code-inspect-binder/osf_g8kbu-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment in your web browser.
   * Please note that Binder may take a few minutes to build the environment.

🚀 **NFDI JupyterHub:** [![NFDI](https://nfdi-jupyter.de/images/nfdi_badge.svg)](https://hub.nfdi-jupyter.de/r2d/gh/code-inspect-binder/osf_g8kbu-f/HEAD?urlpath=rstudio)

   * This will launch the project in an interactive RStudio environment on the NFDI JupyterHub platform.

**Access Downloaded Data:**
The downloaded data from the OSF project is located in the `g8kbu_src` folder.

## Run via Docker for Long-Term Reproducibility

In addition to launching this project using Binder or NFDI JupyterHub, you can reproduce the environment locally using Docker. This is especially useful for long-term access, offline use, or high-performance computing environments.

### Pull the Docker Image

```bash
docker pull meet261/repo2docker-g8kbu-f:latest
```

### Launch RStudio Server

Run the container (with a name, e.g. `rstudio-dev`):
```bash
docker run -it --name rstudio-dev --platform linux/amd64 -p 8888:8787 --user root meet261/repo2docker-g8kbu-f bash
```

Inside the container, start RStudio Server with no authentication:
```bash
/usr/lib/rstudio-server/bin/rserver --www-port 8787 --auth-none=1
```

Then, open your browser and go to: [http://localhost:8888](http://localhost:8888)

> **Note:** If you're running the container on a remote server (e.g., via SSH), replace `localhost` with your server's IP address.
> For example: `http://<your-server-ip>:8888`

## Looking for the Base Version?

For the original Binder-ready repository **without flowR**, visit:
[osf_g8kbu](https://github.com/code-inspect-binder/osf_g8kbu)

