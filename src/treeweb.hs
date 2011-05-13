module Main where

import Graphics.UI.Gtk.WebKit.WebFrame 
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk

main = do
  -- inicjalizacja
  initGUI

  -- tworzymy kontrolkę webkit
  web <- webViewNew
  let loadHome = webViewLoadUri web "http://google.com"
  loadHome
  webViewSetMaintainsBackForwardList web False
  
  -- tworzymy menu
  menu <- hBoxNew False 1
  quit <- buttonNewWithLabel "Quit"
  reload <- buttonNewWithLabel "Reload"
  on reload buttonActivated $ webViewReload web 
  goHome <- buttonNewWithLabel "Home"
  on goHome buttonActivated $ loadHome
  containerAdd menu quit
  containerAdd menu reload
  containerAdd menu goHome

  -- zapełniamy okno główne
  mainCell <- vBoxNew False 1
  containerAdd mainCell menu
  containerAdd mainCell web

  -- pokazujemy wszystko i zapadamy w pętle
  window <- windowNew
  onDestroy window mainQuit
  set window [ containerBorderWidth := 10,
               containerChild := mainCell,
               windowAllowGrow := True ]
  widgetShowAll window
  mainGUI

  return ()