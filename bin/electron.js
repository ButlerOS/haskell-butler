// This is the electron main script.

const { app, ipcMain, BrowserWindow } = require('electron')
const path = require('path')
const fs = require("fs")
const net = require("net")

// Check if the host has an ssh-agent running.
const hasSshAgent = () =>  {
  const ssh_agent = process.env.SSH_AUTH_SOCK
  console.log("Checking for ssh-agent...", ssh_agent)
  return fs.existsSync(ssh_agent)
}

// Connect to the ssh-agent socket
const connectSshAgent = (port) =>  {
  console.log("Connecting to ssh-agent...")
  const client = net.createConnection(process.env.SSH_AUTH_SOCK)
    .on('connect', () =>  { console.log("Connected!"); })
    .on('error', ev =>  { console.log("Error!", ev); })
    .on('close', ev =>  { console.log("Closed!", ev); })
    .on('end', ev =>  { console.log("End!", ev); })
    .on('data', data => {
        console.log("Server:", data)
        port.postMessage(data)
    })
  port.on('message', ev => {
    console.log("Client:", ev.data)
    client.write(ev.data)
  })
  port.start()
}

// enable connecting to self-signed cert
app.commandLine.appendSwitch('ignore-certificate-errors');

app.whenReady().then(() => {
  const win = new BrowserWindow({
    webPreferrences: {
        sandbox: true,
    },
    webPreferences: {
      preload: path.join(__dirname, 'ssh-agent.js'),
    }
  });

  // Hook main side of the ssh-agent preload.
  ipcMain.handle('hasSshAgent', hasSshAgent)
  ipcMain.on('connectSshAgent', (event) =>  { connectSshAgent(event.ports[0]); })

  // Comment to hide dev tools on start.
  win.webContents.openDevTools()

  win.setMenu(null);
  win.loadURL(process.argv[2]);
  if (process.argv.indexOf('--devel') === -1) {
    win.setFullScreen(true);
  }
});
