#include "mainwindow.h"
#include <QApplication>

int main(int argc, char *argv[]) {
#ifdef QT_DEBUG
    qputenv("QTWEBENGINE_REMOTE_DEBUGGING", "23654");
#endif
  QApplication application(argc, argv);
  MainWindow mainWindow;
  mainWindow.show();
  int result = application.exec();
  return result;
}
