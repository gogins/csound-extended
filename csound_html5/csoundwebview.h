#ifndef CSOUNDWEBVIEW_H
#define CSOUNDWEBVIEW_H

#include <QWidget>
#include <QResizeEvent>
#include <QCloseEvent>
#include <QShowEvent>
#include <QUrl>
#include <QMutex>
#include <QtWebEngineWidgets>
#include <QtWebChannel/QtWebChannel>

class CsoundWebView : public QWebEngineView
{
  Q_OBJECT
 public:
  enum BrowserState {
    kNone,
    kCreating,
    kCreated,
  };
  static const QString kUrlBlank;
  CsoundWebView(QWidget* parent = 0);
  virtual ~CsoundWebView();
 public slots:
  QVariant evaluateJavaScript(const QString& scriptSource);
  bool acceptNavigationRequest(const QUrl &url, QWebEnginePage::NavigationType type, bool isMainFrame);
 signals:
  void titleChanged(const QString& title);
  void urlChanged(const QUrl& url);
  void loadStarted();
  void loadFinished(bool ok);
  void navStateChanged(bool canGoBack, bool canGoForward);
  void jsMessage(const QString& name, const QVariantList& args);
protected:
  virtual void resizeEvent(QResizeEvent*);
  virtual void closeEvent(QCloseEvent*);
  virtual void showEvent(QShowEvent*);
  virtual void OnAddressChange(const QString& url);
  virtual void OnTitleChange(const QString& title);
  virtual void SetLoading(bool isLoading);
  virtual void SetNavState(bool canGoBack, bool canGoForward);
  virtual void OnAfterCreated();
};

#endif  // CEFCLIENT_QCEFWEBVIEW_H
