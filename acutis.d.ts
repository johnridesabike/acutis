/**
 *    Copyright 2020 John Jackson
 *
 *   Licensed under the Apache License, Version 2.0 (the "License");
 *   you may not use this file except in compliance with the License.
 *   You may obtain a copy of the License at
 *
 *       http://www.apache.org/licenses/LICENSE-2.0
 *
 *   Unless required by applicable law or agreed to in writing, software
 *   distributed under the License is distributed on an "AS IS" BASIS,
 *   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *   See the License for the specific language governing permissions and
 *   limitations under the License.
 */

export type props = { [key: string]: any };

export type ast = object;

export type renderContext<T> = (
  ast: ast,
  props: props,
  children: { [key: string]: T }
) => T;

export type renderContextSync = renderContext<string>;

export type renderContextAsync = renderContext<Promise<string>>;

export type templateFunction<T> = (
  renderContext: renderContext<T>,
  props: props,
  children: { [key: string]: T }
) => T;

export type templateFunctionSync = templateFunction<string>;

export type templateFunctionAsync = templateFunction<Promise<string>>;

export function makeAst(src: string, name?: string): ast;

export function compile<T>(src: string, name?: string): templateFunction<T>;

export function renderContext(components: {
  [key: string]: templateFunctionSync;
}): renderContextSync;

export function renderContextAsync(components: {
  [key: string]: templateFunctionAsync;
}): renderContextAsync;

export function errorMessage(exn: object): string;
