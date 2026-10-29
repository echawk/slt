# A Linux to test slt's window frontends in, with no GPU or display: Mesa's
# software OpenGL (llvmpipe) and Vulkan (lavapipe), Xvfb for X11 and
# Weston's headless backend for Wayland.  Build and use it with
# scripts/linux-test.
FROM docker.io/library/debian:trixie

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
      sbcl ca-certificates git make \
      libegl1 libegl-mesa0 libgl1-mesa-dri libgles2 libglx-mesa0 \
      libvulkan1 mesa-vulkan-drivers vulkan-tools \
      libwayland-client0 libwayland-egl1 libxkbcommon0 libxkbcommon-x11-0 \
      libx11-6 libx11-xcb1 libxcb1 libglfw3 \
      weston xvfb xauth \
      libfreetype6 fontconfig fonts-dejavu-core fonts-noto-color-emoji \
      libpng16-16t64 zlib1g \
    && rm -rf /var/lib/apt/lists/*

ENV XDG_RUNTIME_DIR=/tmp/runtime
RUN mkdir -p /tmp/runtime && chmod 700 /tmp/runtime
