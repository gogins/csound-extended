#include "mainwindow.h"
#include <QApplication>

/**
 * \page CsoundHtml5
 *
 * This is a basic editor for Csound that supports HTML interfaces and JavaScript programming.
 */

int main(int argc, char *argv[]) {
  qputenv("QTWEBENGINE_REMOTE_DEBUGGING", "8080");
  QApplication application(argc, argv);
  MainWindow main_window;
  if (application.arguments().size() > 1) {
      main_window.loadFile(application.arguments().back());
  }
  main_window.show();
  int result = application.exec();
  return result;
}
