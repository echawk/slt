# vulkan-cffi

CFFI bindings to Vulkan, generated from the Vulkan registry when they are
compiled. Nothing is written by hand per function or structure, so the
bindings cover the chosen API exactly and follow the registry when it is
updated. It does not depend on slt.

The registry is `registry/vk.xml` from
[KhronosGroup/Vulkan-Headers](https://github.com/KhronosGroup/Vulkan-Headers)
v1.4.363 (commit `6802bb4733b63ed5efd3adb308a6c885ef180ea1`), under its own
license (Apache-2.0 OR MIT). To update it, replace that file. The C headers
are not needed.

## What is bound

`src/bindings.lisp` chooses a core version and extensions:

```lisp
(define-vulkan-bindings
  :version "1.3"
  :extensions ("VK_KHR_surface" "VK_KHR_swapchain" "VK_EXT_metal_surface" ...))
```

These generate everything that version and those extensions require, and
every type those refer to. Today that is 592 types and 247 commands, and it
compiles in a few seconds. Structures are laid out by CFFI. The layouts of
all 300 structures and unions were checked against the C headers' `sizeof`
and `offsetof` on arm64 macOS.

## Names

The names are Vulkan's in Lisp's form, in the package `vulkan-cffi`. That
package uses no other package, because `VkFormat` becomes `format`, which
would clash with Common Lisp's.

| Vulkan | Lisp |
| --- | --- |
| `vkCreateInstance` | `create-instance` (a function) |
| `VkInstanceCreateInfo` | `(:struct instance-create-info)` |
| `pApplicationInfo` | `p-application-info` (a slot) |
| `VkFormat`, `VkInstance` | `format`, `instance` (CFFI types) |
| `VK_FORMAT_B8G8R8A8_UNORM` | `+format-b8g8r8a8-unorm+` |
| `VK_KHR_SURFACE_EXTENSION_NAME` | `+khr-surface-extension-name+` |

Dispatchable handles are pointers and the other handles are 64-bit
integers. Commands take and return what the C functions do: pointers for
pointers, and a `result` integer.

## Use

```lisp
(vk:load-vulkan)                      ; opens the loader; global commands
(vk:with-structs ((app vk:application-info
                       :api-version (vk:make-api-version 0 1 3 0))
                  (info vk:instance-create-info :p-application-info app))
  (cffi:with-foreign-object (instance 'vk:instance)
    (vk:check-result (vk:create-instance info (cffi:null-pointer) instance))
    (vk:load-instance-commands (cffi:mem-ref instance 'vk:instance))))
```

- **Loading.** The Vulkan loader is opened by `load-vulkan`, not when the
  system loads, so a program can carry these bindings and still run where
  there is no Vulkan. Commands are called through entry points looked up
  with `vkGetInstanceProcAddr` (`load-instance-commands`) and then
  `vkGetDeviceProcAddr` (`load-device-commands`). A command that is not
  loaded signals an error, and `command-loaded-p` says whether one is.
- **Structures.** `with-struct` zeroes a structure on the stack, sets its
  `sType` (see `structure-type-of`) and stores the slots given.
- **Errors.** `check-result` signals `vulkan-error` for a negative result,
  naming it.

On macOS, through MoltenVK, call it with float traps masked
(`float-features:with-float-traps-masked`). Enable
`VK_KHR_portability_enumeration` and its instance flag to see the device.

Tests: `(asdf:test-system :vulkan-cffi)`. They create an instance and
list the devices when a loader is present.
