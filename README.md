# Simplenote Elm

This is the very beginning of a Simplenote client written in Elm with the goal of running inside Electron.
It is hilariously incomplete at this point.

If you would like to run it, you will need to start by downloading Elm on your computer.

```bash
npm install -g elm
```

Next, clone this repo and install the dependencies after entering the cloned directory.

```bash
elm package install
```

Then, run in the local build server

```bash
elm-reactor
```

And load in your browser at http://localhost:8000/src/Simplenote.elm
At the moment you won't see anything unless you open your developer console to watch the messages flow by.

> Note: You will need to set an access token and client ID in `src/Simplenote.elm` in the `init` function because I haven't yet implemented the ability to login.