const http = require('http');
const functions = require('firebase-functions');
const request = require('request');
const xmlParser = require('xml2js');
const cors = require('cors')({origin: true});

exports.songs = functions.https.onRequest((req, res) => {
  cors(req, res, () => {
    if (queryIsEmpty(req.query)) {
      res.status(200).send([]);
    } else {
      makeApiRequest(req.query)
        .then(songs => res.status(200).send(songs))
        .catch(error => res.status(500).send(error));
    }
  })
});

function queryIsEmpty(query) {
  return !query.search || query.search.length === 0;
}

function makeApiRequest(query) {
  const url = functions.config().songlist.url;
  const queryString = buildQueryString(query);

  return new Promise((resolve, reject) => {
    request(`${url}?${queryString}`, (error, songListRes, body) => {
        if (error) {
          reject(error)
        } else if (songListRes.statusCode !== 200) {
          reject(songListRes);
        } else {
          parseSongListResponse(body).then(resolve).catch(reject);
        }
    });
  });
}

function parseSongListResponse(xml) {
  return new Promise((resolve, reject) => {
    xmlParser.parseString(xml, (error, result) => {
      if (error) {
        reject(error);
      } else {
        const songData = result.xml.songs[0].song || [];
        resolve(songData.map((song) => ({
          code: song.number[0],
          title: song.title[0],
          artist: song.artist[0],
        })));
      }
    });
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
