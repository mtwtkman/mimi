const findDom = selector => document.querySelector(selector)
const getAudioPlayerElement = () => findDom("audio");
const getVolumeSliderElement = () => findDom(".audio-volume-slider");

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

const changeVolume = () => {
  const dom = getAudioPlayerElement();
  const volumeSlider = findDom(".audio-volume-slider");
  const value = parseInt(volumeSlider.value);
  dom.volume = value / 100.0;
};

const changePlaybackRate = value => {
  const dom = getAudioPlayerElement();
  dom.playbackRate = value;
};

const initializeApp = region => {
  const app = Elm.Main.init({ node: region });
  app.ports.play.subscribe(play);
  app.ports.pause.subscribe(pause);
  app.ports.changeVolume.subscribe(changeVolume);
  app.ports.changePlaybackRate.subscribe(changePlaybackRate);
  // app.ports.setCurrentTime.subscribe(updateCurrentTime);
  //app.ports.currentTimeReciever.send(getCurrentTime());
};

initializeApp(document.querySelector("main"));