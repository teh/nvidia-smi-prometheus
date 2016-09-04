# What is this?

This is a job to be run on a server which runs nvidia-smi, reads the
output and re-exports it via prometheus.

This allows you to keep track of your GPU usage.

# Building

```
cabal build
```

# Development

Need: hpack to generate the cabal file from package.yaml
