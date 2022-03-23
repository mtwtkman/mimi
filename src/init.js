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

const setupVolumeEventHandler = app => {
  const updateVolume = (sliderDom, mousePositionX) => {
    const sliderWidth = sliderDom.clientWidth;
    const volumeMaxValue = parseInt(sliderDom.getAttribute("max"));
    const value =  mousePositionX * volumeMaxValue / sliderWidth;
    sliderDom.value = value;
    return value;
  };
  const handler = sliderDom => {
    let isClicked = false;
    return clickState => e => {
      isClicked = clickState(isClicked);
      if (!isClicked) return;
      const currentVolume = updateVolume(sliderDom, e.offsetX);
      app.ports.currentVolumeReciever.send(currentVolume);
    };
  };
  const dom = getVolumeSliderElement();
  const handle = handler(dom);
  dom.addEventListener("mousedown", handle(_ => true), false);
  dom.addEventListener("mousemove", handle(x => x), false)
  dom.addEventListener("mouseup", handle(_ => false), false)
  dom.addEventListener("mouseleave", handle(_ => false), false)
};

const setupAudioNode = app => defaultVolume => {
  const dom = getAudioPlayerElement();
  dom.volume = defaultVolume / 100.0;
  setupVolumeEventHandler(app);
};

const changeVolume = value => {
  const dom = getAudioPlayerElement();
  const volumeSlider = findDom(".audio-volume-slider");
  dom.volume = value / 100.0;
};

const setPlaybackRate = value => {
  const dom = getAudioPlayerElement();
  dom.playbackRate = value;
};

const initializeApp = region => {
  const app = Elm.Main.init({ node: region });
  app.ports.play.subscribe(play);
  app.ports.pause.subscribe(pause);
  app.ports.spawnAudioNode.subscribe(setupAudioNode(app));
  // app.ports.setCurrentTime.subscribe(updateCurrentTime);
  app.ports.changeVolume.subscribe(changeVolume);
  //app.ports.currentTimeReciever.send(getCurrentTime());
};

initializeApp(document.querySelector("main"));