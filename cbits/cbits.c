#include <libsoup/soup.h>
#include <libsoup/soup-gnome.h>
#include <webkit/webkit.h>

#include <gtk/gtk.h>
#include <glib/gi18n.h>
#include <libsoup/soup.h>
#include <webkit/webkit.h>

void spike_setup_webkit_globals()
{
  webkit_set_web_database_directory_path("/tmp/spike.webkit.db");

  SoupSession * ses = webkit_get_default_session();
  //  SoupSessionFeature * jar = soup_session_get_feature(ses, 
  SoupCookieJar * jar = soup_cookie_jar_sqlite_new("/tmp/spike.webkit.cookie.db", 0);
  soup_session_add_feature( ses, SOUP_SESSION_FEATURE (jar));

}
