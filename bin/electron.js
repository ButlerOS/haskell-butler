const { app, BrowserWindow } = require('electron');

// enable connecting to self-signed cert
app.commandLine.appendSwitch('ignore-certificate-errors');

app.whenReady().then(() => {
  const win = new BrowserWindow({
    webPreferrences: {
        sandbox: true,
    }
  });
  win.setMenu(null);
  win.loadURL(process.argv[2]);
  if (process.argv.indexOf('--devel') === -1) {
    win.setFullScreen(true);
  }
});
