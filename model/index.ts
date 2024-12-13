import * as fs from "fs/promises"
import * as ts from "typescript"

/**
 * Takes piped input and creates temp file for for ts.createProgram
 */
const readFromStdIn = async (): Promise<string> => {
	const tempFile = "/tmp/fromTS/stdin.ts"
	const getContents = async () => {
		const stdin = process.stdin
		stdin.setDefaultEncoding("utf8")
		let buffer = ""
		stdin.on("data", (data) => (buffer += data))
		return new Promise<string>((resolve, reject) => {
			stdin.on("end", () => resolve(buffer))
			stdin.on("error", (err) => reject(err))
		})
	}
	const contents = await getContents()
	await fs.writeFile(tempFile, contents)
	return tempFile
}

const getInputFileNames = async () => {
	const fromStdIn = process.argv.slice(2).length < 1
	if (fromStdIn) {
		return [await readFromStdIn()]
	}
	return process.argv.slice(2)
}

const main = async () => {
	const fileNames = await getInputFileNames()
	const program = ts.createProgram(fileNames, {
		target: ts.ScriptTarget.ES2016,
		strict: true,
		alwaysStrict: true,
		noImplicitAny: true,
	})
	const typeChecker = program.getTypeChecker()
	// check if typechecker errors
	const allDiagnostic = ts.getPreEmitDiagnostics(program)
	if (allDiagnostic.length > 0) {
		console.log("Error in typechecker")
		console.log(allDiagnostic)
		return
	}
	const buildTypeAscList = (node: ts.Node): [string, string][] => {
		if (ts.isVariableDeclaration(node)) {
			const type = typeChecker.getTypeAtLocation(node)
			const symbol = typeChecker.getSymbolAtLocation(node.name)
			const name = symbol?.getName()
			if (!name) return []
			const typeString = typeChecker.typeToString(type)
			return [[name, typeString]]
		}
		const children = node.getChildren()
		return children.flatMap(buildTypeAscList)
	}
	const sources = program
		.getSourceFiles()
		.filter((source) => !source.isDeclarationFile)
	const entries = sources.flatMap(buildTypeAscList)
	const typeMap = Object.fromEntries(entries)
	console.log(JSON.stringify(typeMap, null, 4))
}

main()
