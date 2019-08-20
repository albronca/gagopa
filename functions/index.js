const functions = require('firebase-functions');
const request = require('request');
const xmlParser = require('xml2js');
const cors = require('cors')({origin: true});

exports.songs = functions.https.onRequest((req, res) => {
  cors(req, res, () => {
    if (!req.query.search || req.query.search.length === 0) {
      res.status(200).send([]);
    } else {
      const url = functions.config().songlist.url;
      const queryString = buildQueryString(req.query);
      request(`${url}?${queryString}`, (error, songListRes, body) => {
        if (!error && songListRes.statusCode === 200) {
          parseSongListResponse(body, (result) => {
            res.status(200).send(result);
          });
        } else {
          res.status(500).send(`Error: ${error}, Status Code: ${songListRes.statusCode}`);
        }
      });
    }
  })
});

function parseSongListResponse(xml, cb) {
  xmlParser.parseString(xml, (error, result) => {
    const songs = result.xml.songs[0].song || [];
    cb(songs.map((song) => ({
      code: song.number[0],
      title: song.title[0],
      artist: song.artist[0],
    })));
  });
}

function buildQueryString({ search, language }) {
  const params = {
    api: functions.config().songlist.apiname,
    method: 'searchSongs',
    by: 'both',
    per_page: 100,
    language,
    page: 1,
    search: formattedSearch(search)
  };

  return Object.keys(params).reduce((queryString, paramName) => {
    return queryString.concat(`${paramName}=${params[paramName]}&`)
  }, '');
}

function formattedSearch(search) {
  return search.replace(/\s/gi, '+');
}
