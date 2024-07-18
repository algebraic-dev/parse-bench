const chunkSizeLimit = 10000;
const sizeLimit = 520000000;

const generateRandomChunkedRequest = (size) => {
    const method = 'HEAD';
    const path = '/atapo';
    const httpVersion = 'HTTP/1.1';
    const contentType = 'text/plain';

    const generateRandomChunk = () => {
        const chunkSize = Math.floor(Math.random() * chunkSizeLimit) + 1;
        let chunkData = '';
        for (let i = 0; i < chunkSize; i++) {
            chunkData += String.fromCharCode(Math.floor(Math.random() * 26) + 97);
        }
        const chunk = `${chunkSize.toString(16)}\r\n${chunkData}\r\n`;
        process.stdout.write(chunk);
        return chunk.length;
    }

    const generateRandomHeaders= (numHeaders) => {
        const headers = [];
        const headerNames = ['X-Custom-Header', 'X-Request-ID', 'X-Session-ID', 'X-User-ID', 'X-Trace-ID'];
        for (let i = 0; i < numHeaders; i++) {
            const headerName = headerNames[Math.floor(Math.random() * headerNames.length)];
            const headerValue = Math.random().toString(36).substring(2, 15);
            headers.push(`${headerName}: ${headerValue}`);
        }
        return headers.join('\r\n') + '\r\n';
    }

    process.stdout.write(`${method} ${path} ${httpVersion}\r\n`);
    process.stdout.write(`Content-Type: ${contentType}\r\n`);
    process.stdout.write(`Transfer-Encoding: chunked\r\n`);

    const randomHeaders = generateRandomHeaders(Math.floor(Math.random() * 2304));
    process.stdout.write(randomHeaders);
    process.stdout.write(`\r\n`);

    let totalBytesWritten = 0;
    let chunks = 0;

    while (totalBytesWritten < size) {
        const chunkSize = generateRandomChunk();
        chunks += 1;
        totalBytesWritten += chunkSize;
    }

    process.stdout.write(`0\r\n\r\n`);
    process.stderr.write(`chunks written ${chunks}`);
}

generateRandomChunkedRequest(sizeLimit);
