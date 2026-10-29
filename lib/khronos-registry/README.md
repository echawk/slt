# khronos-registry

What [vulkan-cffi](../vulkan-cffi/) and [egl-cffi](../egl-cffi/) share
when they generate bindings from the Khronos API registries (`vk.xml`,
`egl.xml`):

- reading the XML (with plump, as XML rather than HTML);
- the C literals enumerants are written in, such as `(~0U)`, `1000.0F`
  and `EGL_CAST(EGLint,-1)`;
- versions and `depends` expressions;
- member and parameter declarations;
- Lisp names for C ones: `InstanceCreateInfo` is `instance-create-info`.

The registries' schema is described in KhronosGroup/Vulkan-Docs,
`registry.adoc`.
