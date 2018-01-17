#include "mainwindow.h"
#include <QApplication>

int main(int argc, char *argv[]) {
  qputenv("QTWEBENGINE_REMOTE_DEBUGGING", "8080");
  QApplication application(argc, argv);
  MainWindow main_window;
  main_window.show();
  int result = application.exec();
  return result;
}
