const getAudioPlayerElement = () => document.querySelector("audio");

const tick = (start, interval) => {
  setInterval(() => {
  }, interval);
}

const tickBy1Millisec = start => tick(start, 1)

const getCurrentTime = () => {
  const dom = getAudioPlayerElement();
  return dom.currentTime
};

const updateCurrentTime = () => {
  const dom = getAudioPlayerElement();
  // TODO: get current progress bar position and update to new value.
  dom.currentTime = value;
};

const play = () => {
  const dom = getAudioPlayerElement();
  dom.play();
};

const pause = () => {
  const dom = getAudioPlayerElement();
  dom.pause();
};

const setVolume = value => {
  const dom = getAudioPlayerElement();
  dom.volume = value;
};

const setPlaybackRate = value => {
  const dom = getAudioPlayerElement();
  dom.playbackRate = value;
};

const initializeApp = region => {
  const app = Elm.Main.init({ node: region });
  app.ports.play.subscribe(play);
  app.ports.pause.subscribe(pause);
  // app.ports.setCurrentTime.subscribe(updateCurrentTime);
  app.ports.setVolume.subscribe(setVolume);
  //app.ports.currentTimeReciever.send(getCurrentTime());
};

initializeApp(document.querySelector("main"));