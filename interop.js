"use strict";

(() => {
  const SAVE_KEY = "petal-garden-elm-save-v1";
  const BEST_KEY = "petal-garden-elm-best-v1";
  let saved = null;
  let legacy = null;
  let best = 0;
  let warning = "";
  let storageAvailable = true;
  try {
    saved = localStorage.getItem(SAVE_KEY);
    legacy = localStorage.getItem("petal-garden-save-v1");
    const value = localStorage.getItem(BEST_KEY);
    if (value !== null) {
      best = Number(value);
      if (!Number.isSafeInteger(best) || best < 0) {
        best = 0;
        warning = "The saved Elm best score was invalid; it has been reset.";
        console.warn(warning);
      }
    }
  } catch (error) {
    storageAvailable = false;
    warning = "Saving is unavailable. You can play, but this session won't be saved.";
    console.warn(warning, error);
  }
  const app = Elm.Main.init({
    node: document.getElementById("app"),
    flags: {
      seed: crypto.getRandomValues(new Uint32Array(1))[0] & 0x7fffffff,
      saved, legacy, best, warning,
    },
  });
  app.ports.persist.subscribe(({ game, best: bestScore }) => {
    if (!storageAvailable) return;
    try {
      localStorage.setItem(SAVE_KEY, JSON.stringify(game));
      localStorage.setItem(BEST_KEY, String(bestScore));
    } catch (error) {
      storageAvailable = false;
      console.warn("Elm garden save failed:", error);
      app.ports.interopError.send("Saving failed. Keep this page open to continue your current garden.");
    }
  });
  app.ports.openDialog.subscribe((id) => {
    requestAnimationFrame(() => {
      const dialog = document.getElementById(id);
      if (dialog && !dialog.open) dialog.showModal();
    });
  });
  let audio = null;
  let output = null;
  let noiseBuffer = null;
  let generation = 0;
  const sources = new Set();

  function stopSounds() {
    generation++;
    for (const source of sources) source.stop();
  }

  function audioError(error) {
    stopSounds();
    console.warn("Elm garden audio failed:", error);
    app.ports.interopError.send("Audio isn't available. You can keep playing without sound.");
  }

  function track(source, nodes) {
    sources.add(source);
    source.onended = () => {
      sources.delete(source);
      source.disconnect();
      for (const node of nodes) node.disconnect();
    };
  }

  function rustle(start, duration, frequency, volume, sharpness = 0.7) {
    const source = audio.createBufferSource();
    const filter = audio.createBiquadFilter();
    const gain = audio.createGain();
    source.buffer = noiseBuffer;
    filter.type = "bandpass";
    filter.frequency.setValueAtTime(frequency, start);
    filter.frequency.exponentialRampToValueAtTime(frequency * 0.45, start + duration);
    filter.Q.value = sharpness;
    gain.gain.setValueAtTime(0, start);
    gain.gain.linearRampToValueAtTime(volume, start + 0.015);
    gain.gain.exponentialRampToValueAtTime(0.0001, start + duration);
    source.connect(filter).connect(gain).connect(output);
    track(source, [filter, gain]);
    source.start(start);
    source.stop(start + duration);
  }

  function bell(start, frequency, volume, duration = 0.85) {
    // Slightly inharmonic partials give a small wind-chime timbre, not a pure beep.
    for (const [ratio, strength] of [[1, 1], [2.76, 0.2], [5.4, 0.06]]) {
      const source = audio.createOscillator();
      const gain = audio.createGain();
      source.frequency.value = frequency * ratio;
      gain.gain.setValueAtTime(0, start);
      gain.gain.linearRampToValueAtTime(volume * strength, start + 0.008);
      gain.gain.exponentialRampToValueAtTime(0.0001, start + duration);
      source.connect(gain).connect(output);
      track(source, [gain]);
      source.start(start);
      source.stop(start + duration);
    }
  }

  function gardenSound({ action, pop, levelUp }) {
    const start = audio.currentTime + 0.01;
    switch (action) {
      case "plant":
        rustle(start, 0.18, 620, 0.32);
        rustle(start + 0.04, 0.12, 1300, 0.12);
        break;
      case "spade":
        rustle(start, 0.3, 1800, 0.23, 1.2);
        rustle(start + 0.16, 0.16, 440, 0.32);
        break;
      case "butterfly":
        for (let wing = 0; wing < 4; wing++) {
          rustle(start + wing * 0.055, 0.065, 3300, 0.09);
        }
        break;
      case "preview":
        rustle(start, 0.2, 1500, 0.12);
        bell(start + 0.08, 1046.5, 0.035);
        break;
      case "skip":
      case "pop":
        break;
      default:
        throw new Error(`Unknown garden sound: ${action}`);
    }
    const notes = [783.99, 880, 1046.5, 1174.66, 1318.51];
    if (pop > 0) {
      const note = pop - 1;
      const frequency = notes[note % notes.length] * 2 ** Math.floor(note / notes.length);
      rustle(start, 0.23, 2400, 0.16);
      bell(start, frequency, 0.035);
    }
    if (levelUp) {
      const flourish = start + 0.1;
      [0, 2, 3, 4, 2].forEach((note, index) => {
        bell(flourish + index * 0.16, notes[note], 0.055, 1.15);
      });
    }
  }

  app.ports.playSound.subscribe((event) => {
    if (event.action === "mute") {
      stopSounds();
      return;
    }
    try {
      audio ??= new AudioContext();
      if (!output) {
        output = audio.createGain();
        output.gain.value = 0.5;
        output.connect(audio.destination);
        noiseBuffer = audio.createBuffer(1, audio.sampleRate, audio.sampleRate);
        const data = noiseBuffer.getChannelData(0);
        for (let i = 0; i < data.length; i++) data[i] = Math.random() * 2 - 1;
      }
      const pendingGeneration = generation;
      audio.resume().then(() => {
        if (pendingGeneration === generation) gardenSound(event);
      }).catch(audioError);
    } catch (error) { audioError(error); }
  });
})();
